# Shiny web application, 
# Shiny app ####
## Libraries ####
library(shiny)
library(shinyjs)
library(bslib)
library(DBI)
library(RMariaDB)
library(paws)
library(tidyverse)
library(ggplot2)
library(stringr)
library(GGally)
library(plotly)
library(forecast)
library(glue)
library(DT)
library(reticulate)
use_virtualenv("python_env", required = TRUE)
source_python("time_series.py")

## Global variables from database ####
# Get connection to database
region_name = "eu-west-2"
secret_name = "rds!db-68ae85d0-f786-47f3-808d-199d81002c9a"
response <- secretsmanager()$get_secret_value(SecretId = secret_name)
db_string <- response$SecretString
db_credentials <- jsonlite::fromJSON(db_string)

rds <- paws::rds()
instances <- rds$describe_db_instances()
endpoint <- instances$DBInstances[[1]]$Endpoint$Address
port <- instances$DBInstances[[1]]$Endpoint$Port

con <- dbConnect(
  RMariaDB::MariaDB(), 
  dbname = "cryptodb",
  host = endpoint, 
  user=db_credentials$username,
  password=db_credentials$password,
  port = port
)

granularities = dbGetQuery(
  conn = con,
  "SELECT * FROM granularities"
) |>
  # Haven't got these ones yet, maybe in future
  filter(!(granularity %in% c('ONE_MINUTE', 'FIVE_MINUTE'))) |> 
  mutate(
    shiny_granularity = 
      granularity |> 
      str_replace("_", " ")|> 
      str_to_title(),
    shiny_granularity =
      if_else(!str_starts(shiny_granularity, "One"),
              paste0(shiny_granularity, "s"),
              shiny_granularity),
  )
df = dbGetQuery(con, "
    SELECT currency, granularity, start, low, high, close, volume, open
    FROM crypto_values a 
    LEFT JOIN granularities b 
    ON a.granularity_id = b.granularity_id 
    LEFT JOIN currencies c 
    ON a.currency_id = c.currency_id
    WHERE b.granularity <> 'FIVE_MINUTE'
")

currencies = unique(df$currency)
granularities = granularities |> 
  filter(granularity %in% unique(df$granularity))

dbDisconnect(con)

# ui ####
# Define UI for application that draws a histogram
ui <- fluidPage(
  ## Theme ####
  theme = bs_theme(bootswatch = "cyborg"),  # Set the initial theme to Cyborg
  useShinyjs(),  # Enable JavaScript usage
  
  # Custom CSS to adjust the tabset panel width
  tags$style(HTML("
    .custom-tab-panel {
      width: 90%; 
      margin-left: 0; /* Left-align the tab panel */
    }
  ")),
  
  ## Application title ####
  titlePanel(title = "Crypto data"),
  
  ## Sidebar ####
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "curr",
        label = "Select Currency:",
        choices = currencies,
        selected = "BTC-USD",
        multiple = FALSE
      ),
      selectInput(
        "gran", 
        label = "Select Granularity:", 
        choices = granularities$shiny_granularity, 
        selected = "One Hour"
      )
    ),
    
    ## Main Panel ####
    mainPanel(
      div(class = "custom-tab-panel",
          tabsetPanel(
            ### Time series ####
            tabPanel(
              "Time Series plot", 
              radioButtons(
                "plotType", 
                label = "Select Plot Type",
                choices = list("Line Plot" = "line", "Candle Plot" = "candle"),
                selected = "line"
              ),
              plotlyOutput("cryptoPlot", height = "600px")
            ),
            ### Pairs plot ####
            tabPanel(
              "Pairs plot", 
              selectInput(
                "curr_pairs",
                label = "Select Currencies to pair:",
                choices = NULL,
                multiple = TRUE
              ),
              plotOutput("pairsPlot", height = "800px")
            ),
            ### Backtesting ####
            tabPanel(
              "Backtesting",
              p(),
              p("
                This section will run backtests of the models chosen using 
                walk-forward validation
              "),
              p(),
              div("
               In other words, we take a minimum number of observations from 
               the data and forecast one time period ahead. Then we take take 
               either a sliding or expanding window and add on a more recent 
               observation, then again forecast one time period ahead"
              ),
              div("
              Finally press the calculate button to update the plot (note you'll
              also need to press the button again if you change any inputs in
              the sidebar too).
              "),
              p(),
              p(),
              fluidRow(
                column(
                  3, 
                  selectInput(
                    "ts_models",
                    label = "Select models to backtest:",
                    choices = c("auto.arima", "Neural Forecast"),
                    multiple = TRUE
                  )
                ),
                column(
                  4,
                  radioButtons(
                    "backtesttype", 
                    "Choose backtest type:",
                    choices = c("Sliding", "Expanding"),
                    selected = 'Sliding'
                  )
                ),
                column(
                  5,
                  numericInput(
                    "backtestlength", 
                    "For how many points to forecast:", 
                    value = 6, 
                    min = 1, 
                    max = 60
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  div(
                    style = "text-align: right;",# Align the button to the right
                    actionButton("calc_button", "Calculate")
                  )
                )
              ),
              br(), br(),
              #### Backtesting plot ####
              plotlyOutput("backtestPlot", height = "600px"),
              br(), br(),
              #### Backtesting RMSE ####
              DTOutput("backtestTable")
            )
          )
      )
    )
  )
)

# Server ####
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## Choices for paired currencies ####
  # Update based on the initially chosen currency
  observe({
    # Remove the currency chosen for other plots
    available_currencies <- setdiff(currencies, input$curr)
    # Update the secondary choices
    updateSelectInput(session, "curr_pairs", choices = available_currencies)
  })
  
  ## Original data ####
  get_data_filtered_by_granularity = reactive({
    df |>
      filter(
        granularity == 
          granularities$granularity[
            granularities$shiny_granularity == input$gran
          ]
      )
  })
  ## Data filtered by currency ####
  get_data_filtered_by_currency = reactive({
    get_data_filtered_by_granularity() |>
      filter(trimws(currency) %in% input$curr)
  })
  ## Data filtered by multiple currencies ####
  get_data_filtered_by_paired_currencies = reactive({
    get_data_filtered_by_granularity() |>
      filter(trimws(currency) %in% c(input$curr, input$curr_pairs))
  })
  ## Trim data to only plot latest points ####
  # If we plot everything plot gets too big, especially the candle plots
  get_crypto_data = reactive({
    get_data_filtered_by_currency() |> filter(
      start >= case_when(
        input$gran == 'One Day' ~ max(start) - days(90),
        input$gran == 'Six Hours' ~ max(start) - days(30),
        input$gran == 'Two Hours' ~ max(start) - days(10),
        input$gran == 'One Hour' ~ max(start) - days(7),
        input$gran == 'Thirty Minutes' ~ max(start) - days(4),
        input$gran == 'Fifteen Minutes' ~ max(start) - days(2)
      )
    )
  })
  
  ### Plot of crypto data ####
  output$cryptoPlot <- renderPlotly({  # Change renderPlot to renderPlotly
    if (input$plotType == 'line'){
      g <- get_crypto_data() |>
        ggplot(
          aes(x = start, y = open, col = currency)
        ) +
        geom_line() +
        theme_light() +
        xlab(element_blank()) +
        ylab(element_blank()) 
    } else if (input$plotType == 'candle'){
      g <- get_crypto_data() |> 
        mutate(green_or_red = ifelse(open > close, "Red", "Green")) |> 
        ggplot()+
        geom_segment(aes(
          x = start,
          xend = start,
          y = high,
          yend = low),
          colour = 'black'
        ) +
        geom_segment(aes(
          x = start,
          xend =  start,
          y = open,
          yend = close,
          colour = green_or_red),
          size=3, alpha = 0.7) +
        scale_color_manual(values=c("Forest Green","Red")) +
        theme_light() +
        xlab(element_blank()) +
        ylab(element_blank()) 
    }
    ggplotly(g) |> layout(showlegend = FALSE)
  })
  
  ### Pairs plot of two (or more) crypto currencies ####
  output$pairsPlot <- renderPlot({
    if (length(input$curr_pairs) == 0){
      plot(1,1,col="white",type='n',xaxt='n',yaxt='n',axes=FALSE,frame.plot=FALSE,ann=FALSE)
      text(1,1,"Need to select at least two currencies for a pairs plot")
    } else {
      pairs_df = get_data_filtered_by_paired_currencies() |> 
        select(currency, start, open) |> 
        pivot_wider(id_cols=start, names_from=currency, values_from=open) |> 
        select(-start)
      ggpairs(
        pairs_df,
        lower = list(continuous = wrap("points", alpha = 0.7, size = 0.2))
      ) + theme_light()
    }
  })
  
  ## Calculate all predictions for each model ####
  # Reactive expression that only triggers when the button is clicked
  get_pred_df = reactive({
    pred_df = data.frame(
      start = tail(get_crypto_data()$start, input$backtestlength)
    )
    list_pred_df = list()
    ### auto.arima ####
    if ('auto.arima' %in% input$ts_models){
      pred_df = pred_df |> mutate(pred_auto_arima = NA)
      for (i in 1:input$backtestlength){
        y = get_crypto_data()$open
        # Remove points at end which will be forecasted
        x = head(y, -(input$backtestlength - i + 1))
        if (input$backtesttype == 'Sliding'){
          # Remove points at front as it is a sliding backtest
          x = x[i:length(x)]
        }
        if ('auto.arima' %in% input$ts_models){
          pred_df$pred_auto_arima[i] = forecast(auto.arima(x), h=1)$mean
        }
      }
      list_pred_df[['auto.arima']] = pred_df
    }
    
    ### Neural Forecast ####
    if ('Neural Forecast' %in% input$ts_models){
      freq = case_when(
        input$gran == 'One Day' ~ 'D',
        input$gran == 'Six Hours' ~ '6h',
        input$gran == 'Two Hours' ~ '2h',
        input$gran == 'One Hour' ~ 'h',
        input$gran == 'Thirty Minutes' ~ '30min',
        input$gran == 'Fifteen Minutes' ~ '15min'
      )
      ranges = get_crypto_data()$start |> 
        range() |> 
        str_split(" ") |> 
        sapply(first) |> 
        ymd() |> 
        str_remove_all("-")
      # Save output from this as it takes a while and don't want to wait
      done_file_name = glue("nf_{input$curr}_{input$gran}")
      done_file_name = glue("{done_file_name}_{freq}")
      done_file_name = glue("{done_file_name}_{input$backtesttype}")
      done_file_name = glue("{done_file_name}_{input$backtestlength}")
      done_file_name = glue("{done_file_name}_{min(ranges)}_{max(ranges)}")
      if (!file.exists(done_file_name)){
        pred_df = neural_forecast(
          get_crypto_data(),
          input_size = input$backtestlength,
          freq = freq,
          backtest_type = input$backtesttype
        )
        write_csv(pred_df, glue("{done_file_name}.csv"))
      } else {
        pred_df = read_csv(done_file_name, show_col_types = FALSE)
      }
      list_pred_df[['Neural Forecast']] = pred_df
    }
    # Merge the list of data frames into a single data frame
    pred_df = reduce(list_pred_df, full_join, by = 'start')
    pred_df
  })
  
  backtest_results <- eventReactive(input$calc_button, {
    isolate({
      get_pred_df()
    })
  })
  merge_crypto_and_predictions = 
    reactive({
      get_crypto_data() |> 
        left_join(backtest_results(), by = 'start') |> 
        select(start, open, starts_with('pred_')) |> 
        rename(original = open)
    })
  
  
  ### Backtest plots ####
  output$backtestPlot <- renderPlotly({ 
    # Get predicted values
    # pred_df = backtest_results()
    
    withProgress(
      message = "Selected 'Neural Forecast' which can take a few minutes", {
        new_df = merge_crypto_and_predictions() #pred_df)
        
        new_df = new_df |> 
          # tail(min(max(24, 6*input$backtestlength), nrow(get_crypto_data()))) |>
          pivot_longer(
            cols = c(original, starts_with('pred_')), 
            names_to = "type", 
            values_to = "value"
          ) |> 
          drop_na()
        
        g = new_df |> 
          ggplot(aes(x = start, y = value, col = type)) +
          geom_line() +
          theme_light() +
          labs(x = NULL, y = NULL) +
          guides(color = guide_legend(title = NULL))
        ggplotly(g) |>
          layout(
            legend = list(
              orientation = "h",  # Horizontal legend
              x = 0.5,            # Center it horizontally
              y = -0.2,           # Position it below the plot
              xanchor = "center", # Center the legend based on the x value
              yanchor = "top"     # Adjust y-anchor to top for spacing
            )
          )
      })
  })
  
  #### Backtest table ####
  output$backtestTable = renderDT({
    # Get the RMSE for each model
    rmse_df = merge_crypto_and_predictions() |> 
      drop_na() |> 
      pivot_longer(
        cols = starts_with('pred_'), 
        names_to = "type", 
        values_to = "value"
      ) |> 
      drop_na() |> 
      summarise(rmse = Metrics::rmse(value, original), .by = type) |> 
      arrange(rmse)
    datatable(rmse_df, options = list(dom = 't'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
import pandas as pd
import math 
from neuralforecast import NeuralForecast
from neuralforecast.models import NBEATS
from neuralforecast.utils import AirPassengersDF

# https://github.com/Nixtla/neuralforecast/
def neural_forecast(
  data, 
  input_size=None, 
  h=4, 
  max_steps=100, 
  freq='H', 
  backtest_type='Sliding'
  ):
  if not (backtest_type=='Sliding' or backtest_type=='Expanding'):
    raise Exception("`backtest_type` should be one of 'Sliding' or 'Expanding'")
  # Where should the data set start from, 
  data = data.rename(columns={'start': 'ds', 'open': 'y'})
  data = data.assign(unique_id=1)
  data = data[['unique_id','ds','y']]
  pred_dict = dict()
  for i in range(input_size):
    df = data.head(-(input_size-i))
    if backtest_type=='Sliding' and i > 0:
      df = df.tail(-i)
    nf = NeuralForecast(
      models = [NBEATS(
        input_size=round(math.sqrt(len(df))), 
        h=h, 
        max_steps=max_steps
      )],
      freq = freq
    )
    nf.fit(df=df)
    pred_dict[i] = nf.predict().head(1)
  pred_df = pd.concat(pred_dict)
  pred_df = pred_df.rename(columns={'ds':'start', 'NBEATS':'pred_neural_forecast'})
  pred_df = pred_df.reset_index(drop=True)
  return pred_df
  

# nf = NeuralForecast(
#   models = [NBEATS(input_size=24, h=12, max_steps=100)],
#   freq = 'M'
# )
# 
# nf.fit(df=AirPassengersDF)
# nf.predict()

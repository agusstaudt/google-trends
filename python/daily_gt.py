# -*- coding: utf-8 -*-
"""daily_gt.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/11rmdIM39cKVcUob3K3tHrW76nZF6ts-r
"""

def daily_gt(keyword, start, end, inputCategories ,inputCategoriesNames , hl='en-US', tz=360):
  import time
  import pandas as pd 
  import numpy as np # numpy y pandas to data wrangling 
  from datetime import datetime, timedelta # to work w date
  from pytrends.request import TrendReq
  # función para dividir rango de fechas en segmentos 
  def date_range(start, end, intv):
      start = datetime.strptime(start,"%Y-%m-%d")
      end = datetime.strptime(end,"%Y-%m-%d")
      diff = (end  - start ) / intv
      for i in range(intv):
          yield (start + diff * i).strftime("%Y-%m-%d")
      yield end.strftime("%Y-%m-%d")

  # set pytrends para region y zona horaria
  pytrends = TrendReq(hl=hl, tz=tz) 

  # generación de lista de fechas a utilizar
  firstDate = datetime.strptime(start,"%Y-%m-%d")
  lastDate = datetime.strptime(end,"%Y-%m-%d")
  diffDays_control = lastDate - firstDate 
  if  diffDays_control.days >= 90:
    aux = (lastDate - firstDate)/90
    intv = aux.days
    timelist = list(date_range(start, end, intv))
  else:
    timelist = list([start, end])

  # armamos lista vacía para guardar los resultados
  var_dict={}

  # loop de palabras o categorías a importar
  for x in range(0, len(keyword)):
    varName = keyword[x]
    print(f'{x}: {varName}')
    dataset = pd.DataFrame(columns = [varName])
    # loop de rango de fechas sobre cada palabra o categoría
    for i in range(0, len(timelist)-1):
      if timelist[i] != start:
        startAux = datetime.strptime(timelist[i], "%Y-%m-%d") + timedelta(days=1)
        startNew = startAux.strftime("%Y-%m-%d")
      else:
        startAux = datetime.strptime(timelist[i], "%Y-%m-%d")
        startNew = startAux.strftime("%Y-%m-%d")
      print(f'Iteration from {startNew} to {timelist[i+1]}\n')

      if type(varName)==int: # para considerar las categorías por separado
        pytrends.build_payload(kw_list=[''], cat=varName, timeframe=f'{startNew} {timelist[i+1]}') 
        data = pytrends.interest_over_time()
        loc = inputCategories.index(varName)
        catName = inputCategoriesNames[loc] # para renombrar columnas sin nombres en categorías
        data.rename(columns = {f'':f'{catName}'}, inplace = True)

      else:
        pytrends.build_payload(kw_list=[varName], cat=0, timeframe=f'{startNew} {timelist[i+1]}') 
        data = pytrends.interest_over_time()

      if not data.empty: # chequear que la base importada no esté vacía antes de trabajarla, sino pasar a la prox palabra
        data = pd.DataFrame(data.drop(labels=['isPartial'],axis='columns'))
        data['year'] = data.index.year
        data['month'] = data.index.month 
        data['day'] = data.index.day 
        dataset = dataset.append(data)
        del data
        time.sleep(2) # para aumentar el tiempo entre iteración para evitar el eror 429
      else: 
        continue      
    if type(varName)==int:
      dataset = dataset.iloc[:,1:] # elinamos la columna vacía que se genera por importar categorías que no coinciden con el nombre de la primer columna del dataset generado al ppio
      pytrends.build_payload(kw_list=[''], cat=varName, timeframe='all') 
      historical_data = pytrends.interest_over_time()
      historical_data.rename(columns = {f'':f'{catName}_historical'}, inplace = True)
      varName = catName # para que encuentre las columnas para hacer la normalización
    else:
      pytrends.build_payload(kw_list=[varName], cat=0, timeframe='all') 
      historical_data = pytrends.interest_over_time()
      historical_data.rename(columns = {f'{varName}':f'{varName}_historical'}, inplace = True)

    historical_data = pd.DataFrame(historical_data.drop(labels=['isPartial'], axis='columns'))
    historical_data['year'] = historical_data.index.year
    historical_data['month'] = historical_data.index.month 
      
    dataset = pd.merge(dataset, historical_data, on=["year", "month"])
    del historical_data
    dataset[f'{varName}_adjusted'] = dataset[f'{varName}']*(dataset[f'{varName}_historical']/100) 
    var_dict[varName] = pd.DataFrame(dataset)
  return var_dict
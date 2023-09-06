import os 
import pandas as pd 
from functools import reduce


all_files = os.listdir('crypto_rolling_deltaH/')

for w in ['360','720',"1080"]:

    files_window = [ i for i in all_files if w in i]
    df_window_list=[] 
    for file in files_window:
        file_path = os.path.join('crypto_rolling_deltaH/',file)
        df = pd.read_csv(file_path,index_col=[0])
        df_window_list.append(df)
    df_window = reduce(lambda left, right: pd.merge(left, right, on='date'), [file for file in df_window_list])
    df_window.to_csv('deltaH_'+w+".csv",index=False)
    vars= df_window.drop("date",axis=1).columns
    df_window_melt = pd.melt(df_window,id_vars=['date'],
                             value_name='deltaH',var_name='Ticker')
    df_window_melt.to_csv('deltaH_melt_'+w+".csv",index=False)


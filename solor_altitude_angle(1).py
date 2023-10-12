import pandas as pd
import pysolar.solar as sl
import datetime as dt
from os import listdir


def get_data(df): #计算太阳高度角
    year = int(df.split('/')[0])
    day = int(df.split('/')[1])
    month = int(df.split('/')[2].split('-')[0])
    # month = int(df.split('/')[1])
    # day = int(df.split('/')[2].split('-')[0])
    hour = int(df.split(':')[0].split('-')[1]) - 8
    if hour < 0:
        hour = hour + 24
    mimute = int(df.split(':')[1])
    second = int(df.split(':')[-1].split('_')[0])
    lon = float(df.split('_')[1])
    lat = float(df.split('_')[-1])
    t0 = dt.datetime(year, month, day, hour, mimute, second, tzinfo=dt.timezone.utc)
    sun = sl.get_altitude(lat, lon, t0)
    return sun


if __name__ == '__main__':
    path = '/Users/mengqi/Hanyq/SYSU/AEE/贵组项目/林曦/'

    # 一次读取多个文件
    df_list = listdir(path)
    for df in df_list:
        if df != '.DS_Store':  # mac默认有个记录文件位置的文件，需要排除
            if df != 'lsp_a001.csv': # lsp_a001.csv中的年月日写反了，所以单独计算
                print(df)
                df1 = pd.read_csv(path + df)
                df1['info'] = df1['date_time'] + '_' + df1['longitude'].map(str) + '_' + df1['latitude'].map(str) # 把三列数据合并为一列，方便循环
                sun_list = []
                for date in df1['info']:
                    sun = get_data(date)
                    sun_list.append(sun)
                df1['solar_altitude_angle'] = sun_list
                df1.to_csv(path + 'solar_altitude_angle_' + df)

    # 计算单个文件
    # df1 = pd.read_csv(path + 'lsp_a001.csv')
    # df1['info'] = df1['date_time'] + '_' + df1['longitude'].map(str) + '_' + df1['latitude'].map(str)
    # sun_list = []
    # for date in df1['info']:
    #     sun = get_data(date)
    #     sun_list.append(sun)
    # df1['solar_altitude_angle'] = sun_list
    # df1.to_csv(path + 'solar_altitude_angle_lsp_a001.csv')

    print('Done!')
# -*- coding: utf-8 -*-
"""
Extracting tide data using the bpnsdata package of Clea P.
"""

import bpnsdata 
import pandas as pd 
import numpy as np
import geopandas

dir = "C:/Users/arienne.calonge/OneDrive - VLIZ/Ari/PelFish/Apelafico calibration/tide/inputs/AP_Grafton2021/"

geofile = dir + 'AP_Grafton_2021-07-13 02_00_00_2021-07-20 02_00_00.csv'
geodf = geopandas.read_file(geofile,driver="CSV", X_POSSIBLE_NAMES="Longitude", Y_POSSIBLE_NAMES="Latitude")

geodf['datetime'] = pd.to_datetime(geodf['datetime'])

seasurface_info = bpnsdata.SeaSurfaceData()
df = seasurface_info(geodf.loc[0:100], datetime_column='datetime')

df = pd.DataFrame()
steps = 1000
i=1

while i < len(geodf):
    last_sample = i+steps
    if last_sample > len(geodf):
        last_sample = -1
    df_i = seasurface_info(geodf[i:last_sample], datetime_column='datetime')
    df = pd.concat([df, df_i])
    i = last_sample

#reformat df according to Echoview format
df['Depth_date'] = df['datetime'].dt.strftime('%Y-%m-%d').astype(str)
df['Depth_time'] = df['datetime'].dt.strftime('%H:%M:%S').astype(str)
df['Depth_meters'] = 0-df['sea_surface_height_above_sea_level']
df['Depth_status'] = 0

df_ev = df[['Depth_date','Depth_time','Depth_meters','Depth_status']]

  
df_ev.to_csv(dir+"out.csv", index=False, date_format="%Y-%m-%d")

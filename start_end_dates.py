import pandas as pd

df = pd.read_csv('times_rows_pulling_nwis_drb.csv', dtype={'station_id':str})
df.set_index('station_id', inplace=True)
for station in df.index:
    d = pd.read_csv('nwis_discharge_gauges/'+station + '.csv')
    dh = d.head()
    dt = d.tail()
    c = pd.concat([dh, dt])
    dates = pd.to_datetime(c['datetime'])
    df.loc[station, 'start_date'] = dates.min().date()
    df.loc[station, 'end_date'] = dates.max().date()
df.to_csv('drb_stations.csv')


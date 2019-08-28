import pandas as pd
import os
import pytz

df_lists = []
fldr = 'nwis_discharge_gauges/'
# let's just do it for a couple for the last x records
for f in os.listdir(fldr):
    if f.endswith('.csv'):
        site_id = f.split('.csv')[0]
        d = pd.read_csv(fldr+f)
        d['datetime'] = pd.to_datetime(d['datetime'],
                                             utc=True)
        d.set_index('datetime', inplace=True)
        d.index = d.index.tz_convert(pytz.utc)
        # column with index 1 is the qualifiers column
        del d[d.columns[1]]
        x = d.to_xarray()
        df_lists.append(d)

combined = pd.concat(df_lists, axis = 1)
comb_cols = [col.split(':')[1] for col in combined.columns]
combined.columns = comb_cols
x = combined.to_xarray()

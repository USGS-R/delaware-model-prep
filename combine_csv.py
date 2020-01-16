import pandas as pd
import os
import pytz

df_lists = []
fldr = 'nwis_discharge_gauges/'
for f in os.listdir(fldr):
    if f.endswith('.csv'):
        site_id = f.split('.csv')[0]
        d = pd.read_csv(fldr+f)
        print('processing {}, {} rows'.format(site_id, d.shape[0]))
        d['datetime'] = pd.to_datetime(d['datetime'],
                                             utc=True)
        d.set_index('datetime', inplace=True)
        # column with index 1 is the qualifiers column
        del d[d.columns[1]]
        d = d.resample('15T').mean()
        df_lists.append(d)

combined = pd.concat(df_lists, axis = 1)
comb_cols = [col.split(':')[1] for col in combined.columns]
combined.columns = comb_cols
x = combined.to_xarray()
x.to_zarr('drb_streamflow_combined_15_min1')

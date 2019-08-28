import os, sys
sys.path.append(os.path.join(os.path.dirname(__file__), "../preprocess_ml_usgs"))

import geopandas as gpd
from get_basin_attr import get_basin_areas
from get_basin_data import get_data_for_huc
import datetime

def area_to_geo_json(huc, parameter_code):
    areas = get_basin_areas(huc, parameter_code)

    gdf = gpd.read_file('stream_flow_sites.json') 

    gdf.index = gdf.identifier.str.split('-', expand=True)[1]

    gdf['area_sqkm'] = areas

    gdf.to_file('stream_flow_sites_with_area.json', driver='GeoJSON')

def get_flow_data(huc):
    get_data_for_huc(huc, "00060", '1950-01-01',
                     datetime.date.today().strftime("%Y-%m-%d"), service='iv')


# huc for DRB
huc = "020402060105"
# param code for streamflow
parameter_code = "00060"
get_flow_data(huc)





import os
import yaml
import zipfile
# import pandas as pd
# import geopandas as gpd
import urllib

def retrieve_cat_attr_links():
    """
    retrieve a list of links that are for catchment attributes. the links are 
    stored in a yaml file called catchment_attr_links.yaml
    return: [dict] a dictionary where the keys are the categories of attributes
    and the values are the urls for retrieving the data
    """
    link_file = "catchment_attr_links.yaml"
    with open(link_file, 'r') as f:
        links = yaml.safe_load(f)
    return links['categories']

def download_unzip_cat_attr_data(directory, category):
    """
    download files from the urls in the yaml file and then unzip the downloaded
    data
    :param directory: [str] path to the directory where the catchment attribute
    data should be downloaded and unzipped to
    :return: [list] list of file locations of the downloaded and unzipped data
    """
    links = retrieve_cat_attr_links()
    zipped_path = directory + category
    urllib.request.urlretrieve(links[category], zipped_path)
    with zipfile.ZipFile(zipped_path, 'r') as zip_ref:
        zip_ref.extractall(directory)
    os.remove(zipped_path)

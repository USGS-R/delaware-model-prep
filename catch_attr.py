import os
import yaml
import zipfile
import pandas as pd
import geopandas as gpd
import urllib


def retrieve_cat_attr_links(just_categories=False):
    """
    retrieve a list of links that are for catchment attributes. the links are 
    stored in a yaml file called catchment_attr_links.yaml
    :param just_categories: [bool] whether or not to return just the categories
    or the categories with the urls too
    return: [dict] a dictionary where the keys are the categories of attributes
    and the values are the urls for retrieving the data
    """
    link_file = "catchment_attr_links.yaml"
    with open(link_file, 'r') as f:
        links = yaml.safe_load(f)
    if just_categories:
        return links['categories']
    else:
        return links['urls']


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


def combine_categories(cat_data_files, output_file):
    """
    read in and combine all of the data from all catchment attribute category
    files (SSURGO, NLCD, etc.)
    :param cat_data_files: [list] list of paths where the data files are
    :param output_file: [str] path to where the combined file should be written
    """
    # combine all the files
    df_list = []
    for data_file in cat_data_files:
        gdf = gpd.read_file(data_file)
        df = pd.DataFrame(gdf.drop(columns='geometry'))
        df.set_index('hru_id', inplace=True)
        df_list.append(df)
    df_combined = pd.concat(df_list)

    # save to feather
    df_combined.reset_index(inplace=True)
    df_combined.to_feather(output_file)
    return df_combined


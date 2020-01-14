from bs4 import BeautifulSoup
import os
import json
import requests
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


def download_unzip_cat_attr_data(directory, category, region):
    """
    download files from the urls in the yaml file and then unzip the downloaded
    data
    :param directory: [str] path to the directory where the catchment attribute
    data should be downloaded and unzipped to
    :param region: [str] zero-padded number for the region (e.g., '02')
    :param category: [str] the category of parameter (SSURGO, NLCD, etc.)
    :return: [list] list of file locations of the downloaded and unzipped data
    """
    print(directory, category, region)
    zipped_path = directory+'/' + category + region
    download_url = retrieve_file_url(category, region)
    print (download_url)
    urllib.request.urlretrieve(download_url, zipped_path)
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
    df_combined = pd.concat(df_list, axis=1)

    # remove any duplicated columns
    df_combined = df_combined.loc[:, ~df_combined.columns.duplicated()]

    # save to feather
    df_combined.reset_index(inplace=True)
    df_combined.to_feather(output_file)
    return df_combined


def read_sb_json(sb_url):
    """
    read the json data from a sciencbase resource into a dictionary
    :param sb_url: [str] url of the sciencbase resource
    """
    response = requests.get(sb_url)
    return json.loads(response.text)


def retrieve_category_json(category):
    """
    form the url and get the json data for a certain catchment attribute
    category
    """
    url = retrieve_cat_attr_links()[category]
    # make sure we are getting the data back as json
    url += "?format=json"
    data = read_sb_json(url)
    return data


def retrieve_file_url(category, region):
    """
    retrieve the data download url from the science base entry
    :param category: [str] the category of parameter (SSURGO, NLCD, etc.)
    :param region: [str] zero-padded number for the region (e.g., '02')
    """
    category_data = retrieve_category_json(category)
    for file_data in category_data['files']:
        should_end_with = f"{category}_{region}.zip".lower()
        if file_data['name'].lower().endswith(should_end_with):
           return file_data['downloadUri']


def get_metadata_file(category, output_file):
    """
    download the metadata json file for a given category
    :param category: [str] the category of parameter (SSURGO, NLCD, etc.)
    :param output_file: [str] the output file path where the metadata should
    be saved
    """
    category_data = retrieve_category_json(category)
    for file_data in category_data['files']:
        if file_data['originalMetadata']:
            urllib.request.urlretrieve(file_data['downloadUri'], output_file)


def parse_metadata_file(file_path):
    with open(file_path, 'r') as f:
        contents = f.read()
        soup = BeautifulSoup(contents, 'lxml')
    data = []
    for attr in soup.find_all('attr'):
        data_dict = {}
        if attr.attrlabl.text != 'hru_id':
            data_dict['attribute_label'] = attr.attrlabl.text
            data_dict['attribute_definition'] = attr.attrdef.text
            try:
                data_dict['attribute_units'] = attr.attrunit.text
            except AttributeError:
                data_dict['attribute_units'] = 'NA'
        data.append(data_dict)
    return data
    

def consolidate_metdata(metadata_files, output_file):
    combined_data_list = []
    for metadata_file in metadata_files:
        category_data_dict = parse_metadata_file(metadata_file)
        combined_data_list.extend(category_data_dict)
    df = pd.DataFrame(combined_data_list)
    df.dropna(inplace=True)
    df.to_csv(output_file, index=False)
    return df



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
    zipped_path = directory+'/' + category + region
    download_url = retrieve_file_url(category, region)
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
        df.index.rename('hru_reg_id', inplace=True)
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
    """
    combine all of the attribute fields contained in the different category
    files into one file
    :param metadata_files: [list] paths to all the files that will be combined
    :param output_file: [str] path to the output file
    """
    combined_data_list = []
    for metadata_file in metadata_files:
        category_data_dict = parse_metadata_file(metadata_file)
        combined_data_list.extend(category_data_dict)
    df = pd.DataFrame(combined_data_list)
    df.dropna(inplace=True)
    df.to_csv(output_file, index=False)
    return df


def add_nat_id_to_table(nat_reg_table, attr_df, region, join_idx_nat,
                       join_idx_attr, nat_col_names, attr_col_names=None):
    """
    add a column from a nat_reg_table to the attribute table. 
    :param nat_reg_table: [str] the path to the national-regional id look up
    :param attr_file: [str] the path to the catchment attributes_file
    :param region: [str] the region for relating them (e.g., '02') 
    :param join_idx_nat: [str] the col name by which the nat_reg_table should
    be indexed so the indices of that table match the indices of the attr table
    :param join_idx_attr: [str] the col name by which the attribute should be
    indexed so the indices of that table match the indices of the nat_reg table
    :param nat_col_names: [list] the col names from the nat_reg_table that you want
    to add to the attribute table
    :param attr_col_names: [list] the names in the attribute table for the new
    columns. if not included will use the nat_col_names
    :return: updated attr_df
    """
    nat_reg_df = pd.read_csv(nat_reg_table)
    # remove zero padding if there
    nat_reg_df['region'] = nat_reg_df['region'].astype(str).str.lstrip(to_strip='0')
    region = region.strip('0')
    nat_reg_df = nat_reg_df[nat_reg_df['region'] == region]
    nat_reg_df.set_index(join_idx_nat, inplace=True)

    # attr_df = pd.read_feather(attr_file)
    attr_df.set_index(join_idx_attr, inplace=True)
    if attr_col_names:
        attr_df[attr_col_names] = nat_reg_df[nat_col_names]
    else:
        attr_df[nat_col_names] = nat_reg_df[nat_col_names]
    attr_df.reset_index(inplace=True)
    return attr_df



def add_ids_and_seg_attr(nat_reg_seg_file, nat_reg_hru_file, attr_file, region,
                         out_file):
    """
    add the national segment and hru ids to the attribute table as well as a 
    few segment attributes
    :param nat_reg_seg_file: [str] path to file that contains the info about 
    the nationwide river segments
    :param nat_reg_hru_file: [str] path to file that contains the info about 
    the nationwide hrus
    :param attr_file: [str] path to the attribute file that contains only 
    regional hrus and their attributes
    :param region: [str] the region for relating them (e.g., '02') 
    :param out_file: [str] path to where the file with the new columns should
    be saved
    :return: updated attr_file
    """
    attr_df = pd.read_feather(attr_file)
    # add hru_nat_id and seg_reg_id
    attr_df = add_nat_id_to_table(nat_reg_hru_file, attr_df, region,
                                  'hru_id_reg', 'hru_reg_id',
                                  ['hru_id_nat', 'hru_segment'],
                                  ['hru_nat_id', 'seg_reg_id'])

    # add segment attributes
    seg_attr_to_add = ['SUM_LENGTHKM','SUM_TRAV_TIME', 'MAX_CUMDRAINAG']
    attr_df = add_nat_id_to_table(nat_reg_seg_file, attr_df, region,
                                  'seg_id_reg', 'seg_reg_id', seg_attr_to_add)

    # add nat seg_id
    attr_df = add_nat_id_to_table(nat_reg_seg_file, attr_df, region,
                                  'seg_id_reg', 'seg_reg_id', ['seg_id_nat'],
                                  ['seg_nat_id'])

    # convert seg_nat_id Nan to 0
    attr_df['seg_nat_id'].fillna(0, inplace=True)
    attr_df['seg_nat_id'] = attr_df['seg_nat_id'].astype(int)

    attr_df.to_feather(out_file)
    return attr_df


def relate_attr_to_segments(attr_w_id_file, out_file, rm_other_ids=True):
    """
    relate the attributes to the stream segment and remove other id's to avoid
    confusion (unless rm_other_ids is False). The mean of all attributes is 
    taken except for catchment area
    :param attr_w_id_file: [str] path to feather file with attributes and the 
    nation-wide segment id (along with the regional and the HRU's)
    :param out_file: [str] path to where the output file should be written
    :param rm_other_ids: [bool] whether or not you want to drop the other id
    columns
    :return: [pandas df] df of attributes related to national segment id
    """
    attr_df = pd.read_feather(attr_w_id_file)
    by_seg_mean = attr_df.groupby('seg_nat_id').mean()
    by_seg_sum = attr_df.groupby('seg_nat_id').sum()
    # replace the mean cols with sum cols for the appropriate attributes
    sum_cols = ['hru_area']
    by_seg_mean[sum_cols] = by_seg_sum[sum_cols]

    if rm_other_ids:
        non_id_cols = [c for c in by_seg_mean.columns if not c.endswith('_id')]
        assert len(by_seg_mean.columns) - len(non_id_cols) == 3
        by_seg_mean = by_seg_mean[non_id_cols]

    by_seg_mean.reset_index(inplace=True)
    by_seg_mean.to_feather(out_file)
    return by_seg_mean


def subset_for_drb(subset_file, seg_attr_file, out_file):
    """
    subset the segment attributes just for the drb 
    :param subset_file: [str] path to shapefile with the segment subset
    :parm seg_attr_file: [str] path to file with the segment attributes by
    national segment id
    :out_file: [str] path to where the data should be written
    """
    seg_att_df = pd.read_feather(seg_attr_file)
    subset_gdf = gpd.read_file(subset_file)
    seg_subset = seg_att_df.loc[subset_gdf['seg_id_nat']]
    seg_subset.reset_index(inplace=True)
    seg_subset.to_feather(out_file)



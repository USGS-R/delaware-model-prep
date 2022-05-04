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


def download_unzip_cat_attr_data(directory, item_url, category, region="02"):
    """
    download files from the urls in the yaml file and then unzip the downloaded
    data
    :param directory: [str] path to the directory where the catchment attribute
    data should be downloaded and unzipped to
    :param download_url: [str] url for downloading data
    :return: [list] list of file locations of the downloaded and unzipped data
    """
    zipped_path = directory+'/' + category + region
    download_url = retrieve_file_url(item_url, category, region)
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
        df.index.rename('hru_id_reg', inplace=True)
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


def retrieve_item_json(url):
    """
    form the url and get the json data for a certain catchment attribute
    category
    """
    # make sure we are getting the data back as json
    url += "?format=json"
    data = read_sb_json(url)
    return data


def retrieve_file_url(item_url, category, region):
    """
    retrieve the data download url from the science base entry
    :param item_url: [str] url to SB item
    :param category: [str] the category of parameter (SSURGO, NLCD, etc.)
    :param region: [str] zero-padded number for the region (e.g., '02')
    """
    category_data = retrieve_item_json(item_url)
    for file_data in category_data['files']:
        should_end_with = f"{category}_{region}.zip".lower()
        if file_data['name'].lower().endswith(should_end_with):
           return file_data['downloadUri']


def get_metadata_file(item_url, output_file):
    """
    download the metadata json file for a given category
    :param category: [str] the category of parameter (SSURGO, NLCD, etc.)
    :param output_file: [str] the output file path where the metadata should
    be saved
    """
    category_data = retrieve_item_json(item_url)
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
    

def consolidate_metadata(metadata_files, output_file):
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


def add_nat_col_to_table(nat_reg_df, attr_df, region, join_idx_nat,
                         join_idx_attr, nat_col_names, attr_col_names=None):
    """
    add a column from a nat_reg_table to the attribute table. 
    :param nat_reg_table: [dataframe] national-regional id look up df
    :param attr_df: [dataframe] dataframe with segment attributes
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


def add_ids_and_seg_attr(geofab_file, attr_file, region):
    """
    add the national segment and hru ids to the attribute table as well as a 
    few segment attributes
    :param geofab_file: [str] path to national geofabric geodatabase
    :param attr_file: [str] path to the attribute file that contains only 
    regional hrus and their attributes
    :param region: [str] the region for relating them (e.g., '02') 
    :return: updated attr_file
    """
    attr_df = pd.read_feather(attr_file)

    nat_seg_reg_df = gpd.read_file(geofab_file, driver="FileGDB",
                                   layer="nsegmentNationalIdentifier")
    nat_hru_reg_df = gpd.read_file(geofab_file, driver="FileGDB",
                                   layer="nhruNationalIdentifier")

    # add hru_nat_id and seg_reg_id
    attr_df = add_nat_col_to_table(nat_hru_reg_df, attr_df, region,
                                   'hru_id_reg', 'hru_id_reg',
                                   ['hru_id_nat', 'hru_segment'])

    # add nat seg_id
    attr_df = add_nat_col_to_table(nat_seg_reg_df, attr_df, region,
                                   'seg_id_reg', 'hru_segment', ['seg_id_nat'])

    # convert seg_nat_id Nan to 0
    attr_df['seg_id_nat'].fillna(0, inplace=True)
    attr_df['seg_id_nat'] = attr_df['seg_id_nat'].astype(int)

    return attr_df


def weigted_avg(df, weight_col='hru_area'):
    """
    take a weighted average of a dataframe
    :param df: [dataframe] a dataframe for which to take the weighted average
    :param weight_col: [str] column name that contains the weights
    :return: [pandas Series] pandas Series of the average weighted by the
    weight_col
    """
    numerator = (df.multiply(df[weight_col], axis=0)).sum()
    denom = df[weight_col].sum()
    return numerator/denom


def most_rep_cats_by_col(df, col='hru_area',
                         categories=['soil_type', 'cov_type', 'hru_deplcrv']):
    """
    get the most represented categories by column. for example, for the
    defaults i would be getting the soil_type in the dataframe most represented
    by area. i'd be doing the same for cov_type, and hru_deplcrv. this function
    then just averages the rest of the columns (which won't be used anyway)
    :param col: [str] the column to use to judge which one is most represented
    :param categories: [str] the categories to aggregate
    :param df: [dataframe] data frame for which you are getting the most
    :return: [pandas Series] the data with the most represented categories by
    the specified column
    """
    summary_series = df.mean()
    for cat in categories:
        gp = df.groupby(cat).sum()
        most_rep = gp[col].idxmax()
        summary_series.loc[cat] = int(most_rep)
    return summary_series


def aggregate_attr_by_col(attr_df, agg_col):
    """
    aggregate segment or hru attributes by a given column
    :param attr_df: [dataframe] dataframe with segment attributes
    :param agg_col: [str] column name that you want to be grouping your
    """
    by_seg_mean = attr_df.groupby(agg_col).apply(weigted_avg)
    by_seg_sum = attr_df.groupby(agg_col).sum()
    categories = ['soil_type', 'cov_type', 'hru_deplcrv']
    most_rep_cats = attr_df.groupby(agg_col).apply(most_rep_cats_by_col,
                                                   col='hru_area',
                                                   categories=categories)
    assert len(most_rep_cats['soil_type'].unique()) <= 3
    assert sum(~most_rep_cats['soil_type'].isin([1, 2, 3])) == 0
    assert len(most_rep_cats['cov_type'].unique()) <= 4
    assert sum(~most_rep_cats['cov_type'].isin([0, 1, 2, 3])) == 0
    assert len(most_rep_cats['hru_deplcrv'].unique()) <= 2
    assert sum(~most_rep_cats['hru_deplcrv'].isin([1, 2])) == 0

    sum_cols = ['hru_area', 'dprst_area']
    by_seg_mean[sum_cols] = by_seg_sum[sum_cols]
    by_seg_mean[categories] = most_rep_cats[categories]
    del by_seg_mean[agg_col]
    return by_seg_mean


def relate_attr_to_segments(attr_df, rm_other_ids=True):
    """
    relate the attributes to the stream segment and remove other id's to avoid
    confusion (unless rm_other_ids is False). The mean of all attributes is 
    taken except for catchment area
    :param attr_df: [str] dataframe with attributes and the nation-wide segment
    id (along with the regional and the HRU's)
    data by
    :param rm_other_ids: [bool] whether or not you want to drop the other id
    columns
    :return: [pandas df] df of attributes related to national segment id
    """
    agg_df = aggregate_attr_by_col(attr_df, 'seg_id_nat')

    if rm_other_ids:
        cols_not_needed = ['hru_segment', 'hru_id_nat', 'hru_id_reg']
        good_cols = [c for c in agg_df.columns if c not in cols_not_needed]
        agg_df = agg_df[good_cols]

    agg_df.reset_index(inplace=True)
    return agg_df


def subset_by_links(subset_links, attr_df, out_file):
    """
    subset the segment attributes just for the drb 
    :param subset_links: [list] list of links that you want attribute data for
    :param attr_df: [str] dataframe with the segment attributes by national
    segment id
    :param out_file: [str] path to where the data should be written
    """
    attr_df.to_feather('attr_df.feather')
    attr_df.set_index('seg_id_nat', inplace=True)

    # some segments don't have any attributes because they don't have an HRU
    # associated with them.
    # See https://github.com/USGS-R/delaware-model-prep/issues/40
    # So we first filter to the segments (links) that do have attrs and then
    # do `reindex` to include the sites that don't (though they will all have
    # NaN for all values)
    links_with_attrs = subset_links[subset_links.isin(attr_df.index)]
    seg_subset = attr_df.loc[links_with_attrs]
    seg_subset = seg_subset.reindex(subset_links)

    seg_subset.reset_index(inplace=True)
    seg_subset.to_feather(out_file)


def subset_for_drb(drb_file, attr_df, out_file):
    """
    subset the regional attribute file for just the reaches in the drb
    :param drb_file: [str] path to shapefile with the drb segment subset with
    'seg_id_nat' as the links to subset by
    :param attr_df: [str] dataframe with the segment attributes by national
    segment id
    :param out_file: [str] path to where the data should be written
    """
    subset_gdf = gpd.read_file(drb_file)
    link_ids = subset_gdf['seg_id_nat']
    link_ids = link_ids[link_ids != 'NA']
    link_ids = link_ids.astype(int)
    subset_by_links(link_ids, attr_df, out_file)


def download_and_combine_attr_data(out_file):
    """
    download 6 types of catchment attribute data from science base from and
    combine them

    The links are to geodatabases that contain PRMS parameters for hru's and
    streamreachs in the GeoSpatial Fabric. The explanation of this is found
    here: https://wwwbrr.cr.usgs.gov/projects/SW_MoWS/GeospatialFabric.html
    (01-13-2020)

    I found the below links by going to the JSON version of the Science Base
    pages (for example:
    https://www.sciencebase.gov/catalog/item/537a6a01e4b0efa8af081544?format=json,
    for soils)
    
    All of these are for the Region 02 but can be for any of the Regions 01-21
    """
    

    urls = {
        "SSURGO": "https://www.sciencebase.gov/catalog/item/537a6a01e4b0efa8af081544",
        "NLCD2001": "https://www.sciencebase.gov/catalog/item/537a6a10e4b0efa8af081547",
        "NHDPLUSDEM": "https://www.sciencebase.gov/catalog/item/537a6a24e4b0efa8af08154a",
        "Geog": "https://www.sciencebase.gov/catalog/item/537a59ece4b0efa8af081526",
        "MacDNLCD01": "https://www.sciencebase.gov/catalog/item/537a6a51e4b0efa8af081550",
        "Gleeson": "https://www.sciencebase.gov/catalog/item/537a6a33e4b0efa8af08154d"}

    out_directory = "1_network/out/"

    for category, url in urls.items():
        download_unzip_cat_attr_data(out_directory, url, category)

    downloaded_files = [os.path.join(out_directory, f"GeospatialFabricAttributes-PRMS_{category}_02.gdb") for category in urls.keys()]

    combine_categories(downloaded_files, out_file)


def relate_attr_to_drb_segs(attr_file, geofab_file, drb_net_file, out_file):
    """
    relate attributes to drb segments
    :param attr_file: [str] path to combined attribute file
    :param geofab_file: [str] path to national geofabric geodatabase
    :param drb_net_file: [str] path to file describing DRB network
    :returns None:
    """
    attr_df = add_ids_and_seg_attr(geofab_file, attr_file, "02")
    attrs_to_segs = relate_attr_to_segments(attr_df, True)
    subset_for_drb(drb_net_file, attrs_to_segs, out_file)


def fetch_combine_catch_attr_metadata(out_file):
    urls = {
        "SSURGO": "https://www.sciencebase.gov/catalog/item/537a6a01e4b0efa8af081544",
        "NLCD2001": "https://www.sciencebase.gov/catalog/item/537a6a10e4b0efa8af081547",
        "NHDPLUSDEM": "https://www.sciencebase.gov/catalog/item/537a6a24e4b0efa8af08154a",
        "Geog": "https://www.sciencebase.gov/catalog/item/537a59ece4b0efa8af081526",
        "MacDNLCD01": "https://www.sciencebase.gov/catalog/item/537a6a51e4b0efa8af081550",
        "Gleeson": "https://www.sciencebase.gov/catalog/item/537a6a33e4b0efa8af08154d"}

    out_directory = "1_network/out/"
    category_metadata_files = []

    for category, url in urls.items():
        category_out_file = os.path.join(out_directory, f"{category}_metadata.xml")
        get_metadata_file(url, category_out_file)
        category_metadata_files.append(category_out_file)
        
    consolidate_metadata(category_metadata_files, out_file)


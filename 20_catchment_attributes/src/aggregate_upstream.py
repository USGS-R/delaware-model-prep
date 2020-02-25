import pandas as pd
from catch_attr import aggregate_attr_by_col

def add_from_in_to_reach(us_link_df):
    """
    adding the from_reach in the to_reach column to make sure we are
    aggregating the US basins includeing the one of interest
    :param us_link_df: [dataframe] dataframe with two columns, 'from_reach' and
    'to_reach'
    :return: [dataframe] datafraom with the addition of the 'from_reach's in 
    the 'to_reach's
    example:
        before:
            "from_reach", "to_reach"
            d, c
            d, a
            d, b
            e, d
            e, c
            e, b
            e, a
        after:
            "from_reach", "to_reach"
            d, c
            d, a
            d, b
            d, d
            e, d
            e, c
            e, b
            e, a
            e, e
    """
    unique_from = us_link_df['from_reach'].unique()
    new_df = pd.DataFrame([unique_from, unique_from]).T
    new_df.columns = us_link_df.columns
    us_link_df = us_link_df.append(new_df)
    return us_link_df


def aggregate_upstream_attr(seg_attr_file, upstream_link_file, out_file):
    """
    aggregate the attributes of all the upstream reaches for a given stream
    reach and save as feather file
    :param seg_attr_file: [str] path to file with the segment attributes by
    national segment id
    :param upstream_link_file: [str] path to csv file that has the list of 
    national reach ids and all of the national reach ids upstream of eachs
    reach id with two columns, 'from_reach' and 'to_reach'.
    For example:
        "from_reach", "to_reach"
        d, c
        d, a
        d, b
        e, d
        e, c
        e, b
        e, a
    :param out_file: [str] path to feather file where the data will be written
    :return: None
    """
    # read in us link file
    us_link_df = pd.read_csv(upstream_link_file)

    # process us_link_df
    us_link_df = add_from_in_to_reach(us_link_df)
    # get no nans b/c nan is where there
    us_link_no_nan = us_link_df[~us_link_df['to_reach'].isna()]
    # make sure 'to_reach' is integer
    us_link_no_nan['to_reach'] = us_link_no_nan['to_reach'].astype(int)
    # make list of US reaches for each DS reach
    us_ds_list_ser = us_link_no_nan.groupby('from_reach')['to_reach'].apply(list)

    # read in attribute file
    attr_df = pd.read_feather(seg_attr_file)
    # set index so that the join will work
    attr_df.set_index('seg_id_nat', inplace=True)

    # join the two dfs
    df_list = []
    for ds_seg_id in us_ds_list_ser.index:
        ser_seg = us_ds_list_ser.loc[[ds_seg_id]]
        df_exp = ser_seg.explode().reset_index()
        df_exp.set_index('to_reach', inplace=True)
        attr_df_seg = attr_df.loc[df_exp.index]
        attr_df_seg = attr_df_seg.join(df_exp)
        # aggregate by the 'from_reach' col
        agg_df = aggregate_attr_by_col(attr_df_seg, 'from_reach')
        agg_df.reset_index(inplace=True)
        df_list.append(agg_df)

    combined_df = pd.concat(df_list, ignore_index=True)
    combined_df = combined_df.rename(columns={"from_reach": "seg_id_nat"})
    combined_df.to_feather(out_file)
    return combined_df


import pandas as pd
from catch_attr import most_rep_cats_by_col, weigted_avg

sample_seg_ids = [1, 1, 1, 2, 2, 2, 3, 3]
sample_hru_area = [2, 1, 5, 2, 4, 2, 5, 6]
sample_hru_elev = [4, 6, 8, 1, 5, 3, 1, 9]
sample_soil_type = [1, 2, 2, 7, 3, 7, 1, 4]
data = {'seg_id': sample_seg_ids,
        'hru_area': sample_hru_area,
        'hru_elev': sample_hru_elev,
        'soil_type': sample_soil_type}
sample_df = pd.DataFrame(data)
df1 = sample_df[sample_df['seg_id'] == 1]
df2 = sample_df[sample_df['seg_id'] == 2]
df3 = sample_df[sample_df['seg_id'] == 3]

def test_most_rep_cat_per_col():
    most_rep1 = most_rep_cats_by_col(df1, categories=['soil_type'])
    soil_type_seg_id1 = 2
    assert most_rep1['soil_type'] == soil_type_seg_id1

    most_rep2 = most_rep_cats_by_col(df2, categories=['soil_type'])
    soil_type_seg_id2 = 3
    assert most_rep2['soil_type'] == soil_type_seg_id2

    most_rep3 = most_rep_cats_by_col(df3, categories=['soil_type'])
    soil_type_seg_id3 = 4
    assert most_rep3['soil_type'] == soil_type_seg_id3


def test_weighted_avg():
    wgt_avg1 = weigted_avg(df1)
    assert wgt_avg1['hru_elev'] == 6.75

    wgt_avg2 = weigted_avg(df2)
    assert wgt_avg2['hru_elev'] == 3.5

    wgt_avg3 = weigted_avg(df3)
    assert round(wgt_avg3['hru_elev'], 3) == 5.364

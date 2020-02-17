import os
from catch_attr import relate_attr_to_segments


def test_agg_to_seg():
    attr_w_id_file = 'tests/sample_combined_cats_w_ids_02.feather'
    random_segs = [3356, 2768, 2317, 1714, 1646]
    temp_outfile = 'temp_out'
    seg_attr = relate_attr_to_segments(attr_w_id_file, temp_outfile)
    seg_attr.set_index('seg_id_nat', inplace=True)

    var = 'soil_moist_max'
    assert round(seg_attr.loc[3356, var], 2) == 5.26
    assert round(seg_attr.loc[2768, var], 2) == 3.71
    assert round(seg_attr.loc[2317, var], 2) == 2.93
    assert round(seg_attr.loc[1714, var], 2) == 2.92
    assert round(seg_attr.loc[1646, var], 2) == 3.91

    var = 'hru_area'
    assert round(seg_attr.loc[3356, var]) == 17874
    assert round(seg_attr.loc[2768, var]) == 39272
    assert round(seg_attr.loc[2317, var]) == 3448
    assert round(seg_attr.loc[1714, var]) == 25884
    assert round(seg_attr.loc[1646, var]) == 26228

    var = 'dprst_area'
    assert round(seg_attr.loc[3356, var]) == 0
    assert round(seg_attr.loc[2768, var]) == 339
    assert round(seg_attr.loc[2317, var]) == 3
    assert round(seg_attr.loc[1714, var], 1) == 2.5
    assert round(seg_attr.loc[1646, var]) == 1357


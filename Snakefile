# import global scripts
import sys
import os

# add all the src directories to the path so that we have access to the scripts
def add_all_src_dir():
    for contents in os.walk('.'):
        directory = contents[0]
        if directory.endswith('src'):
            scripts_path =  os.path.abspath("20_catchment_attributes/src")
            sys.path.insert(0, scripts_path)

# import local scripts
import catch_attr

configfile: 'catchment_attr_links.yaml'
regions = ['02']
cat_att_dir = '20_catchment_attributes/out'

rule all:
    input:
        metadata = f"{cat_att_dir}/combined_metadata.csv",
        seg_attr = expand("{fldr}/combined_seg_attr_{region}.feather", fldr=cat_att_dir, region=regions),
        drb_attr = f"{cat_att_dir}/seg_attr_drb.feather",
        subset_drb_attr = f"{cat_att_dir}/seg_attr_drb_subset.feather"

params_out_fmt = "{fldr}/GeospatialFabricAttributes-PRMS_{category}_{region}.gdb"
rule get_all_catchment_params:
    output:
        directory(params_out_fmt)
    run:
        catch_attr.download_unzip_cat_attr_data(wildcards.fldr,
                                                wildcards.category,
                                                wildcards.region)

rule combine_categories:
    input:
        'catchment_attr_links.yaml',
        expand(params_out_fmt, fldr=cat_att_dir, category=config['categories'],
               region=regions)
    output:
        "{fldr}/combined_categories_{region}.feather"
    run:
        catch_attr.combine_categories(input[1:], output[0])

rule add_ids_and_seg_attributes:
    input:
        '10_spatial_data/out/nsegmentNationalIdentifier.csv',
        '10_spatial_data/out/nhruNationalIdentifier.csv',
        rules.combine_categories.output
    output:
        "{fldr}/combined_cats_w_ids_{region}.feather"
    run:
        catch_attr.add_ids_and_seg_attr(input[0], input[1], input[2], wildcards.region, output[0])

rule relate_to_segments:
    input:
        rules.add_ids_and_seg_attributes.output
    output:
        "{fldr}/combined_seg_attr_{region}.feather"
    run:
        catch_attr.relate_attr_to_segments(input[0], output[0], True)

rule subset_attr_to_drb:
    input:
        # this is a shapefile of the entire DRB cutout
        "10_spatial_data/out/Segments_subset.shp",
        f"{cat_att_dir}/combined_seg_attr_02.feather"
    output:
        rules.all.input.drb_attr
    run:
        catch_attr.subset_for_drb(input[0], input[1], output[0])

rule subset_attr_drb_subset:
    input:
        # this is a list of link ids that are in the subset of the DRB (the small subset Xiaowei started with)
        "10_spatial_data/out/drb_subset_links.csv",
        f"{cat_att_dir}/combined_seg_attr_02.feather"
    output:
        rules.all.input.subset_drb_attr
    run:
        catch_attr.subset_for_drb_subset(input[0], input[1], output[0])

metadata_file_fmt = "{fldr}/{category}_metadata.xml"
rule get_metadata_xml_files:
    output:
        metadata_file_fmt
    run:
        catch_attr.get_metadata_file(wildcards.category, output[0])

rule combine_metadata_files:
    input:
        metadata_files = expand(metadata_file_fmt, fldr=cat_att_dir,
               category=config['categories'])
    output:
        rules.all.input.metadata
    run:
        catch_attr.consolidate_metdata(input, output[0])



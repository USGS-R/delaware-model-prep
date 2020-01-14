import catch_attr

configfile: 'catchment_attr_links.yaml'
regions = ['02']
cat_att_dir = 'cat_attr'

rule all:
    input:
        combined_data = expand("{fldr}/combined_categories_{region}.feather",
                               fldr=cat_att_dir, region=regions),
        metadata = f"{cat_att_dir}/combined_metadata.csv"


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
        rules.all.input[0]
    run:
        catch_attr.combine_categories(input[1:], output[0])

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


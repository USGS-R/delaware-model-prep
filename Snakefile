import catch_attr

configfile: 'catchment_attr_links.yaml'
cat_att_dir = 'cat_attr/'
cat_attr_out = "cat_attr/GeospatialFabricAttributes-PRMS_{cat}_02.gdb"

rule all:
    input:
        f"{cat_att_dir}combined_categories.feather"


rule get_all_cat_params:
    output:
        directory(expand(cat_attr_out, cat=config['categories']))
    run:
        catch_attr.download_unzip_cat_attr_data(cat_att_dir, wildcards.cat)

rule combine_categories:
    input:
        'catchment_attr_links.yaml',
        rules.get_all_cat_params.output
    output:
        f"{cat_att_dir}combined_categories.feather"
    run:
        catch_attr.combine_categories(input[1:], output[0])

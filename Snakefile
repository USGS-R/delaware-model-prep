import catch_attr

configfile: 'catchment_attr_links.yaml'
cat_att_dir = 'cat_attr/'
cat_attr_out = "cat_attr/GeospatialFabricAttributes-PRMS_{cat}_02.gdb"

rule all:
    input:
        expand(cat_attr_out, cat=config['categories'])

rule get_all_cat_params:
    output:
        directory(cat_attr_out)
    run:
        catch_attr.download_unzip_cat_attr_data(cat_att_dir, wildcards.cat)

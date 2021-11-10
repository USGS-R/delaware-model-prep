# The purpose of these functions is to interface between
# the scipiper pipeline and the Python code
download_and_combine_sb_attr_data <- function(out_ind) {
    reticulate::use_condaenv("rgcn")
    out_file <- as_data_file(out_ind)
    catch_attr_py <-
        reticulate::import_from_path('catch_attr', path = '1_network/src/')
    catch_attr_py$download_and_combine_attr_data(out_file)
    gd_put(out_ind)
}


relate_attr_to_drb_segs <-
    function(out_ind, all_attr, geofabric, drb_network) {
        reticulate::use_condaenv("rgcn")
        out_file <- as_data_file(out_ind)
        attr_file <- as_data_file(all_attr)
        geofab_file <- as_data_file(geofabric)
        drb_net_file <- as_data_file(drb_network)
        
        catch_attr_py <-
            reticulate::import_from_path('catch_attr', path = '1_network/src/')
        catch_attr_py$relate_attr_to_drb_segs(
            out_file = out_file,
            attr_file = attr_file,
            geofab_file = geofab_file,
            drb_net_file = drb_net_file
        )
        gd_put(out_ind)
    }


fetch_combine_catch_attr_metadata <- function(out_ind) {
    reticulate::use_condaenv("rgcn")
    out_file <- as_data_file(out_ind)
    catch_attr_py <-
        reticulate::import_from_path('catch_attr', path = '1_network/src/')
    catch_attr_py$fetch_combine_catch_attr_metadata(out_file)
    gd_put(out_ind)
}

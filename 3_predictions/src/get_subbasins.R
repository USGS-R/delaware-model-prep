


get_subbasins = function(ind_file,
                         subbasin_outlet_file,
                         model_run_loc,
                         gd_config = 'lib/cfg/gd_config.yml'){

  # read in subbasin outlets from yaml file
  subbasin_outlets = yaml::read_yaml(subbasin_outlet_file)$subbasin_outlets

  # find out which basins are nested in other basins - we have to remove segments from downstream basin that
  #   overlaps with segments from upstream basins
  col_upstream_rows = is_b_upstream_of_a(seg_id_nat_a = subbasin_outlets,
                                         seg_id_nat_b = subbasin_outlets,
                                         model_run_loc = model_run_loc)

  subbasins = list()  # store subbasins in list
  for(cur_basin_outlet in subbasin_outlets){
    upstream_basin_outlets = colnames(col_upstream_rows)[which(col_upstream_rows[rownames(col_upstream_rows) ==
                                                                                   cur_basin_outlet])]

    upstream_basin_outlets = upstream_basin_outlets[upstream_basin_outlets != cur_basin_outlet]

    if(length(upstream_basin_outlets) == 0){ # if no upstream basins
      cur_basin = create_subbasin(subbasin_seg_id_nat = get_upstream_segs(seg_id_nat = cur_basin_outlet,
                                                                          model_run_loc = model_run_loc),
                                  model_run_loc = model_run_loc)

      subbasins = c(subbasins, list(cur_basin))
      names(subbasins)[which(cur_basin_outlet == subbasin_outlets)] = eval(cur_basin_outlet)
    }else{ # otherwise there are upstream basins, so remove the overlaps with nested subbasins
      upstream_subbasin_segs = unlist(get_upstream_segs(seg_id_nat = upstream_basin_outlets,
                                                        model_run_loc = model_run_loc)) %>% unique()
      upstream_segs = get_upstream_segs(seg_id_nat = cur_basin_outlet,
                                        model_run_loc = model_run_loc) #
      upstream_segs = upstream_segs[!upstream_segs %in% upstream_subbasin_segs] # remove upstream subbasin segs

      cur_basin = create_subbasin(subbasin_seg_id_nat = upstream_segs,
                                  model_run_loc = model_run_loc)

      subbasins = c(subbasins, list(cur_basin))
      names(subbasins)[which(cur_basin_outlet == subbasin_outlets)] = eval(cur_basin_outlet)    }
  }


  data_file = as_data_file(ind_file)
  saveRDS(object = subbasins, file = data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

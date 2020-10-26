
#'
#'
#'
gather_sntemp_output = function(ind_file,
                                model_run_loc,
                                model_output_file,
                                model_fabric_file,
                                sntemp_vars,
                                gd_config = 'lib/cfg/gd_config.yml'){

  stream_temp_intermediates = get_sntemp_intermediates(model_output_file = file.path(model_run_loc,
                                                                                     model_output_file),
                                                       model_fabric_file = file.path(model_run_loc,
                                                                                     model_fabric_file),
                                                       sntemp_vars = sntemp_vars)

  # conversion based on PRMS-SNTemp output; standardizing to metric - see 3_predictions/notes/model_variable_description.txt for output variable details
  inches_to_m = 0.0254
  cfs_to_m3sec = 1/3.28084^3
  langleys_day_to_w_m2 = 11.63/24 # see https://www.nrcs.usda.gov/wps/portal/nrcs/detailfull/null/?cid=stelprdb1043619 for conversion
  stream_temp_intermediates = stream_temp_intermediates %>%
    mutate(parameter_value = case_when(parameter == 'seg_rain' ~ parameter_value * inches_to_m,
                                       parameter == 'seg_outflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seginc_gwflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seginc_sroff' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seginc_ssflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seg_upstream_inflow' ~ parameter_value * cfs_to_m3sec,
                                       parameter == 'seg_potet' ~ parameter_value * inches_to_m,
                                       parameter == 'seginc_swrad' ~ parameter_value * langleys_day_to_w_m2,
                                       TRUE ~ parameter_value))


  stream_temp_intermediates_wide = stream_temp_intermediates %>%
    spread(key = 'parameter', value = 'parameter_value')

  # get static variables
  hru_mapping = read.table(file.path(model_run_loc, 'input/myparam.param'), skip = 4, stringsAsFactors = F)

  seg_length = hru_mapping[(grep('seg_length',hru_mapping[,1])+5):(grep('seg_length',hru_mapping[,1])+460),] # seg_length, units are in m
  seg_length = tibble(model_idx = as.character(seq(1,456)), seg_length = as.numeric(seg_length)) # in meters

  seg_slope = hru_mapping[(grep('seg_slope',hru_mapping[,1])+5):(grep('seg_slope',hru_mapping[,1])+460),] # seg_slope, units dimensionless
  seg_slope = tibble(model_idx = as.character(seq(1,456)), seg_slope = as.numeric(seg_slope)) # dimensionless

  seg_elev = hru_mapping[(grep('seg_elev',hru_mapping[,1])+5):(grep('seg_elev',hru_mapping[,1])+460),] # seg_elev, units are in m
  seg_elev = tibble(model_idx = as.character(seq(1,456)), seg_elev = as.numeric(seg_elev)) # in meters


  stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_length, by = 'model_idx')
  stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_slope, by = 'model_idx')
  stream_temp_intermediates_wide = left_join(stream_temp_intermediates_wide, seg_elev, by = 'model_idx')


  out_file = as_data_file(ind_file)
  feather::write_feather(x = stream_temp_intermediates_wide, path = out_file)
  gd_put(ind_file)
}

# gather output by subbasin
gather_sntemp_output_subbasin = function(ind_file,
                                         subbasin_ind_file,
                                         full_data_ind_file,
                                         subbasin_outlet_id,
                                         gd_config = 'lib/cfg/gd_config.yml'){

  stream_temp_intermediates_wide = feather::read_feather(sc_retrieve(full_data_ind_file, 'getters.yml'))

  subbasins = readRDS(sc_retrieve(subbasin_ind_file, 'getters.yml'))

  cur_subbasin = subbasins[subbasin_outlet_id][[subbasin_outlet_id]]
  cur_seg_id_nats = as.character(cur_subbasin$seg_id_nat)

  # subset network for PGDL

  stream_temp_intermediates_wide_sub = stream_temp_intermediates_wide %>%
    dplyr::filter(seg_id_nat %in% cur_seg_id_nats)

  out_file = as_data_file(ind_file)
  feather::write_feather(x = stream_temp_intermediates_wide_sub, path = out_file)
  gd_put(ind_file)
}




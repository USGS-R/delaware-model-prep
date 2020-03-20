

subset_sntemp_preds = function(ind_file,
                               sub_net_file,
                               full_data_ind,
                               gd_config = 'lib/cfg/gd_config.yml'){

  stream_temp_intermediates_wide = feather::read_feather(sc_retrieve(full_data_ind))

  # subset set of seg_id_nats
  sub_net = read.csv(sub_net_file)
  sub_net_sites = as.character(unique(c(sub_net$from_reach, sub_net$to_reach)))

  stream_temp_intermediates_wide_sub = stream_temp_intermediates_wide %>%
    dplyr::filter(seg_id_nat %in% sub_net_sites)

  length(unique(stream_temp_intermediates_wide_sub$seg_id_nat))

  out_file = as_data_file(ind_file)
  feather::write_feather(x = stream_temp_intermediates_wide_sub, path = out_file)
  gd_put(ind_file)
}


aggregate_sntemp_preds = function(ind_file,
                                  sub_net_file,
                                  subset_data_file,
                                  gd_config = 'lib/cfg/gd_config.yml'){

  sub_net = read.csv(sub_net_file)

  subbasin_outlets = unique(sub_net$from_reach)

  subset_data = feather::read_feather(subset_data_file)

  vars = colnames(subset_data)[4:ncol(subset_data)]


  out = lapply(subbasin_outlets, function(cur_basin){
    segs = c(cur_basin, sub_net$to_reach[sub_net$from_reach == cur_basin])

    cur_agg_data = dplyr::filter(subset_data, seg_id_nat %in% segs)  %>%
      group_by(date) %>%
      summarise_at(vars(seg_ccov:seg_elev), mean) %>%
      ungroup() %>%
      mutate(seg_tave_water = subset_data$seg_tave_water[subset_data$seg_id_nat == cur_basin],
             seg_outflow = subset_data$seg_outflow[subset_data$seg_id_nat == cur_basin],
             subbasin_outlet_seg_id_nat = cur_basin)
  }) %>% bind_rows()

  out_file = as_data_file(ind_file)
  feather::write_feather(x = out, path = out_file)
  gd_put(ind_file)
}



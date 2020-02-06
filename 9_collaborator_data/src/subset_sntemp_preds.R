

subset_sntemp_preds = function(ind_file,
                               sub_net_file,
                               full_data_file,
                               gd_config = 'lib/cfg/gd_config.yml'){

  stream_temp_intermediates_wide = feather::read_feather(full_data_file)

  # subset set of seg_id_nats
  sub_net = read.csv(sub_net_file)
  sub_net_sites = as.character(unique(c(sub_net$from_reach, sub_net$to_reach)))

  stream_temp_intermediates_wide_sub = stream_temp_intermediates_wide %>%
    dplyr::filter(seg_id_nat %in% sub_net_sites)

  length(unique(stream_temp_intermediates_wide_sub$seg_id_nat))

  out_file = as_data_file(ind_file)
  feather::write_feather(x = stream_temp_intermediates_wide_sub, path = out_file)
  gd_put(remote_ind = ind_file, local_source = out_file, config_file = gd_config)
}




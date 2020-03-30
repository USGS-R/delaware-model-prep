

subset_sntemp_preds = function(out_file,
                               sub_net_file,
                               full_data_ind,
                               gd_config = 'lib/cfg/gd_config.yml'){

  stream_temp_intermediates_wide <- feather::read_feather(sc_retrieve(full_data_ind))

  # subset set of seg_id_nats
  sub_net <- read_csv(sub_net_file, col_types='ccdd')
  sub_net_sites <- unique(sub_net$seg_id_nat)

  stream_temp_intermediates_wide_sub <- stream_temp_intermediates_wide %>%
    dplyr::filter(seg_id_nat %in% sub_net_sites) %>%
    mutate(seg_tave_upstream = ifelse(seg_tave_upstream == -98.9, NA, seg_tave_upstream)) # convert numeric flag to NA

  feather::write_feather(x = stream_temp_intermediates_wide_sub, path = out_file)
}


aggregate_sntemp_preds = function(ind_file,
                                  sub_net_file,
                                  subset_data_file,
                                  gd_config = 'lib/cfg/gd_config.yml'){

  sub_net <- readr::read_csv(sub_net_file, col_types='ccdd')
  subbasin_outlets <- unique(sub_net$outlet)

  subset_data <- feather::read_feather(subset_data_file)

  # area-weighted mean. hrus_area is the total area of HRUs contributing directly to the segment for each row
  awmean <- function(values, hrus_area) {}
  # stream-reach-length-weighted mean
  lwmean <- function(values, seg_length) {}

  out = lapply(subbasin_outlets, function(cur_basin){
    segs = c(cur_basin, sub_net$to_reach[sub_net$from_reach == cur_basin])

    cur_agg_data = dplyr::filter(subset_data, seg_id_nat %in% segs)  %>%
      group_by(date) %>%
      mutate(is_outlet = seg_id_nat == cur_basin) %>%
      summarise_at(
        # from https://www.usgs.gov/media/videos/precipitation-runoff-modeling-system-prms-streamflow-modules:
        # "Variables with names having the prefix “seg” are flows for each segment plus all associated upstream segments." (but...really??)
        # "Variables with names having the prefix “seginc” are flows for each segment. These are the incremental flows in the stream network."
        basin_ccov = awmean(seg_ccov, hrus_area), # unitless; area-weighted average cloud cover fraction for each segment from HRUs contributing flow to the segment
        basin_humid = awmean(seg_outflow), # area-weighted average relative humidity for each segment from HRUs contributing flow to the segment
        basin_rain = awmean(seg_rain), # area-weighted average rainfall for each segment from HRUs contributing flow to the segment
        basin_shade = awmean(seg_shade), # seems to be always 0. area-weighted average shade fraction for each segment
        basin_tave_air = awmean(seg_tave_air), # area-weighted air temperature for each segment from HRUs contributing flow to the segment
        basin_tave_gw = awmean(seg_tave_gw), # C, groundwater temperature
        basin_tave_sroff = awmean(seg_tave_sroff), # surface runoff temperature
        seg_tave_ss = awmean(seg_tave_ss), # subsurface temperature
        #seg_tave_upstream = mean(seg_tave_upstream), # Temperature of streamflow entering each segment
        #seg_upstream_inflow = mean(seg_upstream_inflow), # cfs; sum of inflow from upstream segments
        network_width = lwmean(seg_width, seg_length), # m, width of each segment
        outlet_width = seg_width[is_outlet], # m, width of each segment
        seginc_gwflow = awmean(seginc_gwflow), # cfs, area-weighted average groundwater discharge for each segment from HRUs contributing flow to the segment
        seginc_potet = awmean(seginc_potet), # area-weighted average potential ET for each segment from HRUs contributing flow to the segment
        seginc_sroff = awmean(seginc_sroff), # area-weighted average surface runoff for each segment from HRUs contributing flow to the segment
        seginc_ssflow = awmean(seginc_ssflow), # area-weighted average interflow for each segment from HRUs contributing flow to the segment
        seginc_swrad = awmean(seginc_swrad), # W m^-2; area-weighted average solar radiation for each segment from HRUs contributing flow to the segment
        network_length = sum(seg_length), # m?; Length of each segment
        network_slope = lwmean(seg_slope, seg_length), # Slope of segments
        basin_elev = awmean(seg_elev, hrus_area), # m?; Elevation of each segment, used to estimate atmospheric pressure
        # outputs
        outlet_tave_water = seg_tave_water[is_outlet], # Computed daily mean stream temperature for each segment
        outlet_outflow = seg_outflow[is_outlet] # cfs?; streamflow leaving a segment (total discharge at the segment outlet)
        # hru_elev?
        # hru_slope?
      ) %>%
      ungroup() %>%
      mutate(seg_tave_water = subset_data$seg_tave_water[subset_data$seg_id_nat == cur_basin],
             seg_outflow = subset_data$seg_outflow[subset_data$seg_id_nat == cur_basin],
             subbasin_outlet_seg_id_nat = cur_basin)
  }) %>% bind_rows()

  out_file = as_data_file(ind_file)
  feather::write_feather(x = out, path = out_file)
  gd_put(ind_file)
}



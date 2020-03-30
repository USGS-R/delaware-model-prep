

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
    # convert numeric flag to NA. from
    # https://github.com/nhm-usgs/prms/blob/2ca4e2d298fa853bcc87f28343cedceaad30ef27/prms/stream_temp.f90#L905-L907:
    # "-98.9 is the code for no flow on this timestep"
    mutate(seg_tave_upstream = ifelse(seg_tave_upstream == -98.9, NA, seg_tave_upstream))

  feather::write_feather(x = stream_temp_intermediates_wide_sub, path = out_file)
}


aggregate_sntemp_preds = function(ind_file,
                                  sub_net_file,
                                  subset_data_file,
                                  gd_config = 'lib/cfg/gd_config.yml'){

  sub_net <- readr::read_csv(sub_net_file, col_types='ccdd')
  subbasin_outlets <- unique(sub_net$outlet)

  subset_data <- feather::read_feather(subset_data_file)

  out = lapply(subbasin_outlets, function(cur_basin){
    segs <- sub_net$seg_id_nat[sub_net$outlet == cur_basin]

    cur_agg_data = dplyr::filter(subset_data, seg_id_nat %in% segs) %>%
      left_join(filter(sub_net, outlet == cur_basin), by='seg_id_nat') %>%
      group_by(date) %>%
      mutate(is_outlet = seg_id_nat == cur_basin) %>%
      # Prefix definitions from https://www.usgs.gov/media/videos/precipitation-runoff-modeling-system-prms-streamflow-modules:
      #   "Variables with names having the prefix “seg” are flows for each segment plus all associated upstream segments." (but...really??)
      #   "Variables with names having the prefix “seginc” are flows for each segment. These are the incremental flows in the stream network."
      # in summarize() below, line comments describe the value being summarized (not the resulting summary).
      # "table" refers to https://water.usgs.gov/water-resources/software/PRMS/PRMS_tables_5.0.0.pdf.
      summarize(
        # drivers
        basin_ccov = weighted.mean(seg_ccov, hrus_area_km2), # fraction, but not in table; area-weighted average cloud cover fraction for each segment from HRUs contributing flow to the segment
        basin_humid = weighted.mean(seg_humid, hrus_area_km2), # fraction (0-1), in table as "humidity"; area-weighted average relative humidity for each segment from HRUs contributing flow to the segment
        basin_rain = weighted.mean(seg_rain, hrus_area_km2), # not in table, but basin_rain and hru_rain are in inches; area-weighted average rainfall for each segment from HRUs contributing flow to the segment
        basin_shade = weighted.mean(seg_shade, hrus_area_km2), # seems to be always 0. area-weighted average shade fraction for each segment
        basin_tave_air = weighted.mean(seg_tave_air, hrus_area_km2), # area-weighted air temperature for each segment from HRUs contributing flow to the segment
        basin_gwflow = weighted.mean(seginc_gwflow, hrus_area_km2), # cfs; area-weighted average groundwater discharge for each segment from HRUs contributing flow to the segment
        basin_potet = weighted.mean(seginc_potet, hrus_area_km2), # inches; area-weighted average potential ET for each segment from HRUs contributing flow to the segment
        basin_sroff = weighted.mean(seginc_sroff, hrus_area_km2), # cfs; area-weighted average [SUM??] surface runoff for each segment from HRUs contributing flow to the segment
        basin_ssflow = weighted.mean(seginc_ssflow, hrus_area_km2), # cfs; area-weighted average [SUM??] interflow for each segment from HRUs contributing flow to the segment
        basin_swrad = weighted.mean(seginc_swrad, hrus_area_km2), # langleys or W m^-2?; area-weighted average solar radiation for each segment from HRUs contributing flow to the segment
        # values we probably won't use as predictors (too internal to the temperature module):
        basin_tave_gw = weighted.mean(seg_tave_gw, hrus_area_km2), # assuming C; groundwater temperature
        basin_tave_sroff = weighted.mean(seg_tave_sroff, hrus_area_km2), # assuming C; surface runoff temperature
        basin_tave_ss = weighted.mean(seg_tave_ss, hrus_area_km2), # assuming C; subsurface temperature
        # basin properties
        basin_area = sum(hrus_area_km2),
        network_slope = weighted.mean(seg_slope, seg_length_km), # (not in table. hru_slope is decimal fraction, 0 to 10); Slope of segments
        network_length = sum(seg_length_km), # km; Length of each segment
        network_width = weighted.mean(seg_width, seg_length_km), # m, width of each segment
        network_elev = weighted.mean(seg_elev, seg_length_km), # m?; Elevation of each segment, used to estimate atmospheric pressure
        # outlet properties (these are actually properties of the most downstream segment in the network - close enough to consider "outlet", I hope)
        outlet_slope = seg_slope[is_outlet], # m, width of each segment
        outlet_width = seg_width[is_outlet], # m, width of each segment
        outlet_elev = seg_elev[is_outlet], # m?; Elevation of each segment, used to estimate atmospheric pressure
        # targets for prediction
        outlet_tave_water = seg_tave_water[is_outlet], # assuming C; Computed daily mean stream temperature for each segment
        outlet_outflow = seg_outflow[is_outlet], # cfs?; streamflow leaving a segment (total discharge at the segment outlet)
        # metadata
        outlet_seg_id_nat = cur_basin
      ) %>%
      ungroup()
  }) %>% bind_rows()

  out_file = as_data_file(ind_file)
  feather::write_feather(x = out, path = out_file)
  gd_put(ind_file)
}

# Inspect the outputs to make sure the ranges are reasonable
# raw_preds <- feather::read_feather('9_collaborator_data/psu/sntemp_preds_high_obs_sites.feather')
# agg_preds <- feather::read_feather('9_collaborator_data/psu/sntemp_preds_aggregated.feather')
# # basin_ccov seems fine, ranges from 0.25 to 0.8
# raw_preds$seg_ccov %>% summary
# agg_preds$basin_ccov %>% summary
# ggplot(agg_preds, aes(x=date, y=basin_ccov, color=outlet_seg_id_nat)) + geom_line()
# # CHECK AFTER REBUILD basin_humid is boring, always 0.7
# raw_preds$seg_humid %>% summary
# agg_preds$basin_humid %>% summary
# ggplot(agg_preds, aes(x=date, y=basin_humid, color=outlet_seg_id_nat)) + geom_line()
# # basin_rain
# raw_preds$seg_rain %>% summary
# agg_preds$basin_rain %>% summary
# ggplot(agg_preds, aes(x=date, y=basin_humid, color=outlet_seg_id_nat)) + geom_line() + facet_wrap(~ outlet_seg_id_nat)
# # basin_shade
# # basin_tave_air
# # basin_gwflow
# # basin_potet
# # basin_sroff
# # basin_ssflow
# # basin_swrad
# # basin_tave_gw
# # basin_tave_sroff
# # basin_tave_ss
# # basin_area
# # network_slope
# # network_length
# # network_width
# # network_elev
# # outlet_slope
# # outlet_width
# # outlet_elev
# # outlet_tave_water
# # outlet_outflow
# # outlet_seg_id_nat

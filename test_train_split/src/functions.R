water_year_to_days <- function(year) {
  seq(from = as.POSIXct(paste0(year - 1, "-10-01"), tz = 'UTC'),
      to = as.POSIXct(paste0(year, "-09-30"), tz = 'UTC'), by="+1 day")
}

summarize_holdout <- function(df) {
  df %>%
    group_by(seg_id_nat, subseg_id) %>%
    summarize(fraction_heldout = sum(in_time_holdout) / n(),
              n_data_points = n())
}

all_inf_or_zero <- function(x) {
  all(x == 0 | is.infinite(x))
}

min_abs_not_zero <- function(x) {
  no_zeros <- x[x != 0]
  no_zeros[which.min(abs(no_zeros))]
}

#' Compute various metrics about nearby monitored reaches
#' Look at obs for closest monitored reach by fish distance,
#' observations for reaches with radius in m by fish and bird distance
#' @param x numeric Row of updown distance matrix or bird distance matrix (probably via apply function)
#' @param radius numeric Radius in meters to check for observations within, by bird and fish distance
#' @param obs_summary data.frame Data frame of number of flow and temp obs by subseg
#' @param reach_endpoints_matrix matrix a distance matrix created from the network vertices data frame, filtered to
#' reach mouth vertices
get_distance_metrics <- function(x, radius, obs_summary, col_name_suffix) {
  obs_summary <- ungroup(obs_summary)

  x_within_radius <- x[abs(x) < radius & x != 0]
  closest_reach <- min_abs_not_zero(x)

  obs_summary_radius <- obs_summary %>%
    filter(subseg_id %in% names(x_within_radius)) %>%
    summarize(n_obs_flow_radius = sum(n_flow),
              n_obs_temp_radius = sum(n_temp),
              n_subsegs_temp_flow_radius = n()) %>%
    mutate(subsegs_within_radius = list(x_within_radius))
  #what if no reaches match the above filters
  if(nrow(obs_summary_radius)  == 0) {
    obs_summary_radius <- tibble(n_obs_flow_radius = NA,
                                 n_obs_temp_radius = NA,
                                 n_subsegs_temp_flow_radius = NA)
  }
  final_tibble <- obs_summary_radius %>%
    mutate(closest_reach_id = names(closest_reach),
           closest_reach_dist = closest_reach) %>%
    left_join(select(obs_summary, subseg_id, n_temp, n_flow),
              by = c(closest_reach_id = 'subseg_id')) %>%
              # by = setNames(nm = paste('closest_reach_id', col_name_suffix, sep = '_'),
              #               object = 'subseg_id')) %>%
    rename(n_temp_closest = n_temp, n_flow_closest = n_flow) %>%
    rename_with(.fn = ~ paste(., col_name_suffix, sep = '_'))
  return(final_tibble)
}


#' Get list of pre-specified segments of interest
get_key_segments <- function(){
  list(
    neversink_seg_ids = c('1645', '1638'),
    beltzville_seg_ids = c('1703', '1697'),
    christina_seg_ids = c('2007', '2012', '2013', '2014', '2037'),
    lordville_id = '1573',
    cannonsville_seg_ids = c('1566', '1562', '1561', '1560', '1559', '1557'),
    pepacton_seg_ids = c('1449', '1447', '1448', '1446', '1445', '1438'),
    trenton_seg_id = '1498',
    montague_seg_id = '1659'
  )
}

#could put these in a yaml?  Or in a single list object that would be more easily re-used
#' appends names of segments of interest to a dataframe based on seg_id_nat
append_key_seg_names <- function(df) {
  key_segments <- get_key_segments()
  delaware_mainstem_segments_df <- get_delaware_mainstem_sites()
  df %>%
    mutate(key_seg = case_when(seg_id_nat %in% key_segments$neversink_seg_ids ~ 'Neversink reservoir',
                               seg_id_nat %in% key_segments$beltzville_seg_ids ~ 'Beltzville reservoir',
                               seg_id_nat %in% key_segments$christina_seg_ids ~ 'Christina basin',
                               seg_id_nat %in% key_segments$lordville_id ~ 'Delaware @Lordville',
                               seg_id_nat %in% key_segments$cannonsville_seg_ids ~ 'Cannonsville reservoir',
                               seg_id_nat %in% key_segments$pepacton_seg_ids ~ 'Pepacton Reservoir',
                               seg_id_nat %in% key_segments$trenton_seg_id ~ 'Delaware @Trenton',
                               seg_id_nat %in% key_segments$montague_seg_id ~ 'Delaware @Montague',
                               seg_id_nat %in% delaware_mainstem_segments_df$seg_id_nat ~ 'Delaware mainstem',
                               TRUE ~ NA_character_))
}

#' Searches DRB filtered sites for names starting with Delaware
get_delaware_mainstem_sites <- function() {
  site_summary <- readRDS('2_observations/out/drb_filtered_sites.rds') %>%
    filter(grepl(pattern = '^USGS-', x = site_id)) %>%
    mutate(usgs_site_id = gsub(pattern = '^USGS-', replacement = '', x = site_id))
  site_info <- readNWISsite(site_summary$usgs_site_id)
  delaware_sites <- site_info %>%
    filter(grepl(pattern = '^delaware', ignore.case = TRUE, x = station_nm)) %>%
    mutate(site_id = paste0('USGS-', site_no)) %>%
    left_join(site_summary, by = 'site_id')
}

#' produce heat map of site observations through time (Y = years, x = sites)
#' @param subseg_ids char subseg_ids to include in plot
#' @param obs_df data frame of daily observations; will be summarized to annual observations per subseg_id
#' @param min_year don't plot data before this year
#' @param holdout_years numeric Will be highlighted in heatmap
#' @param subseg_order_df data frame of subseg_id and order columns, used to set order in plot
#' @param title char plot title
reach_time_range_plot <- function(subseg_ids, obs_df, min_year, holdout_water_years, subseg_order_df,
                                  title = NA, lwd = 1) {
  subseg_df <- obs_df %>%
    filter(subseg_id %in% subseg_ids)
  subseg_df_year <- subseg_df %>%
    mutate(water_year = calcWaterYear(date)) %>%
    group_by(water_year, subseg_id) %>%
    summarize(n_obs = n()) %>%
    filter(water_year >= min_year) %>%
    mutate(holdout_water_year = water_year %in% holdout_water_years) %>%
    left_join(subseg_order_df, by = 'subseg_id')
  ggplot(subseg_df_year, aes(x = reorder(subseg_id, order), y = water_year)) +
    geom_tile(aes(fill = n_obs, col = holdout_water_year), lwd = lwd) +
    scale_color_manual(values = c(`FALSE` = NA, `TRUE` = 'green')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, y = 'Water year', x = 'subseg_id')
}

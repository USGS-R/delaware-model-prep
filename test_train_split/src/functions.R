year_to_days <- function(year) {
  seq(from = as.POSIXct(paste0(year, "-01-01"), tz = 'UTC'),
      to = as.POSIXct(paste0(year, "-12-31"), tz = 'UTC'), by="+1 day")
}

summarize_holdout <- function(df) {
  df %>%
    group_by(seg_id_nat) %>%
    summarize(fraction_heldout = sum(in_time_holdout) / n(),
              n_data_points = n())
}

all_inf_or_zero <- function(x) {
  all(x == 0 | is.infinite(x))
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
  # #closest pos/neg to zero
  # min_dist <- postive_dist[which.min(positive_dist)]
  # max_dist <- negative_dist[which.max(negative_dist)]
  # out_tibble <- tibble(pos_dist_to_nearest = min_dist,
  #                      nearest_pos_reach = )

}



min_abs_not_zero <- function(x) {
  no_zeros <- x[x != 0]
  no_zeros[which.min(abs(no_zeros))]
}

#could put these in a yaml?  Or in a single list object that would be more easily re-used
append_key_seg_names <- function(df) {
  neversink_seg_ids <- c('1645', '1638')
  beltzville_seg_ids <- c('1703', '1697')
  christina_seg_ids <- c('2007', '2012', '2013', '2014', '2037')
  lordville_id <- '1573'
  cannonsville_seg_ids <- c('1566', '1562', '1561', '1560', '1559', '1557')
  pepacton_seg_ids <- c('1449', '1447', '1448', '1446', '1445', '1438')
  trenton_seg_id <- '1498'
  montague_seg_id <- '1659'
  df %>%
    mutate(key_seg = case_when(seg_id_nat %in% neversink_seg_ids ~ 'Neversink reservoir',
                               seg_id_nat %in% beltzville_seg_ids ~ 'Beltzville reservoir',
                               seg_id_nat %in% christina_seg_ids ~ 'Christina basin',
                               seg_id_nat %in% lordville_id ~ 'Delaware @Lordville',
                               seg_id_nat %in% cannonsville_seg_ids ~ 'Cannonsville reservoir',
                               seg_id_nat %in% pepacton_seg_ids ~ 'Pepacton Reservoir',
                               seg_id_nat %in% trenton_seg_id ~ 'Delaware @Trenton',
                               seg_id_nat %in% montague_seg_id ~ 'Delaware @Montague',
                               TRUE ~ NA_character_))
}

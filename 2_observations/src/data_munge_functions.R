subset_data <- function(out_ind, crosswalk_ind, dat_ind) {
  
  crosswalk <- readRDS(sc_retrieve(crosswalk_ind))
  
  basin_dat <- readRDS(sc_retrieve(dat_ind)) %>%
    filter(site_id %in% crosswalk$site_id)
  
  saveRDS(basin_dat, as_data_file(out_ind))
  gd_put(out_ind, as_data_file(out_ind))
    
}

generate_site_summary <- function(dat_ind, crosswalk_ind, out_ind) {
  dat <- readRDS(sc_retrieve(dat_ind)) %>%
    group_by(site_id) %>%
    summarize(n_obs = n())
  
  delaware_pts <- dat %>%
    left_join(readRDS(sc_retrieve(crosswalk_ind))) %>%
    mutate(
      dist_to_reach_km = bird_dist_to_subseg_m/1000,
      nobsBin = base::cut(n_obs, breaks=c(min(n_obs), 10, 100, 1000, max(n_obs)+1), right=FALSE)) %>%
    dplyr::select(
      site_id,
      latitude,
      longitude,
      dist_to_reach_km,
      matched_seg_id_nat = seg_id_nat,
      matched_subseg_id = subseg_id,
      n_obs,
      nobsBin)
  
  saveRDS(delaware_pts, as_data_file(out_ind))
  gd_put(out_ind, as_data_file(out_ind))
    
}
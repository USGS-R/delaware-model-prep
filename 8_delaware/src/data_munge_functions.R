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
      matched_reach_id = seg_id_nat,
      n_obs,
      nobsBin)
  
  saveRDS(delaware_pts, as_data_file(out_ind))
  gd_put(out_ind, as_data_file(out_ind))
    
}

generate_site_geojson <- function(summary_ind, out_ind) {
  
  dat_write <- readRDS(sc_retrieve(summary_ind)) %>%
    st_as_sf(coords = c('longitude', 'latitude')) %>%
    st_set_crs(4326) %>%
    sf::st_transform(crs = 4326) %>%
    geojsonsf::sf_geojson()
  
  geojsonio::geojson_write(dat_write, file = as_data_file(out_ind), convert_wgs84 = TRUE)
  gd_put(out_ind, as_data_file(out_ind))
  
}
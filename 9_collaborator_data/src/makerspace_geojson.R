generate_site_geojson <- function(summary_ind, out_ind) {
  
  dat_write <- readRDS(sc_retrieve(summary_ind)) %>%
    dplyr::select(site_id, latitude, longitude, dist_to_reach_km, matched_reach_id = matched_seg_id_nat, n_obs, nobsBin) %>%
    st_as_sf(coords = c('longitude', 'latitude')) %>%
    st_set_crs(4326) %>%
    sf::st_transform(crs = 4326) %>%
    geojsonsf::sf_geojson()
  
  geojsonio::geojson_write(dat_write, file = as_data_file(out_ind), convert_wgs84 = TRUE)
  gd_put(out_ind)
  
}
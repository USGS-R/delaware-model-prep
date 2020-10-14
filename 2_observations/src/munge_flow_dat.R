# munge flow dat
get_flow_sites <- function(flow_ind, temp_sites_ind, out_ind) {
  flow_dat <- readRDS(sc_retrieve(flow_ind, 'getters.yml')) %>%
    distinct(site_id) %>% pull(site_id)

  flow_sites <- paste0('USGS-', flow_dat)

  # find sites not in temperature data
  temp_sites <- readRDS(sc_retrieve(temp_sites_ind, 'getters.yml')) %>%
    distinct(site_id) %>% pull(site_id)

  flow_missing <- lubridate::setdiff(flow_sites, temp_sites)

  flow_site_meta <- dataRetrieval::whatNWISsites(sites = gsub('USGS-', '', flow_missing))

  saveRDS(flow_site_meta, as_data_file(out_ind))
  gd_put(out_ind)

}

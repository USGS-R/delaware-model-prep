# munge flow dat
get_flow_sites <- function(flow_ind, temp_sites_ind, out_ind) {
  flow_dat <- readRDS(sc_retrieve(flow_ind, 'getters.yml')) %>%
    distinct(site_id) %>% pull(site_id)
  flow_sites <- paste0('USGS-', flow_dat)
  # find sites not in temperature data
  temp_sites <- readRDS(sc_retrieve(temp_sites_ind, 'getters.yml')) %>%
    as_tibble() %>%
    distinct(site_id) %>% pull(site_id)

  flow_missing <- lubridate::setdiff(flow_sites, temp_sites)

  flow_site_meta <- dataRetrieval::whatNWISsites(sites = gsub('USGS-', '', flow_missing))

  saveRDS(flow_site_meta, as_data_file(out_ind))
  gd_put(out_ind)

}

munge_split_flow <- function(dat_ind, sites_ind, holdout_water_years,
                             holdout_reach_ids, out_ind) {
  flow_dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))

  drb_sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml'))
  dat_drb <- flow_dat %>%
    mutate(site_id = sprintf('USGS-%s', site_id),
           discharge_cms = round(flow_cfs / 35.314666, 3)) %>%
    select(-flow_cfs) %>%
    filter(!is.na(discharge_cms)) %>%
    filter(site_id %in% unique(drb_sites$site_id)) %>%
    left_join(distinct(drb_sites, site_id, seg_id_nat, subseg_id), by='site_id') %>%
    group_by(seg_id_nat, subseg_id, date) %>%
    summarize(discharge_cms = mean(discharge_cms),
              site_id = paste(site_id, collapse = ';')) %>%
    ungroup() %>%
    mark_time_space_holdouts(holdout_water_years, holdout_reach_ids)

  saveRDS(dat_drb, as_data_file(out_ind))
  gd_put(out_ind)
}

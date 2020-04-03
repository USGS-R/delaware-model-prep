subset_sites <- function(out_ind, crosswalk_ind, dat_ind, fish_dist, bird_dist) {
  
  crosswalk <- readRDS(sc_retrieve(crosswalk_ind))
  
  basin_sites <- crosswalk %>%
    mutate(bird_filtered = bird_dist_to_subseg_m < bird_dist,
      fish_filtered = abs(fish_dist_to_outlet_m) < fish_dist) %>%
    filter(bird_filtered, fish_filtered) %>%
    st_drop_geometry %>%
    distinct() # should check into why we need this
  
  saveRDS(basin_sites, as_data_file(out_ind))
  gd_put(out_ind)
    
}

filter_temp_data <- function(cross_ind, dat_ind, out_ind) {
  
  sites <- readRDS(sc_retrieve(cross_ind)) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct(site_id, subseg_id, seg_id_nat, .keep_all = TRUE)
  dat <- readRDS(sc_retrieve(dat_ind))
  
  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct()
  
  saveRDS(drb_dat, as_data_file(out_ind))
  gd_put(out_ind)
  
}

munge_temp_dat <- function(sites_ind, dat_ind, out_ind) {
  
  sites <- readRDS(sc_retrieve(sites_ind)) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct()
  dat <- readRDS(sc_retrieve(dat_ind))
  
  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, temp_degC, .keep_all = TRUE) %>%
    group_by(site_id, date) %>%
    summarize(temp_C = mean(temp_degC)) %>%
    ungroup()
  
  drb_dat <- drb_dat %>%
    left_join(sites) %>%
    group_by(subseg_id, seg_id_nat, date) %>%
    summarize(temp_c = mean(temp_C)) %>%
    ungroup()
  
  saveRDS(drb_dat, as_data_file(out_ind))
  gd_put(out_ind)
  
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
  gd_put(out_ind)
    
}

munge_flow <- function(dat_ind, sites_ind, out_ind) {
  ddat_wide <- readr::read_csv(sc_retrieve(dat_ind), 
                               col_types=cols(.default=col_double(), datetime=col_date(format='')))
  
  drb_sites <- readRDS(sc_retrieve(sites_ind))
  
  ddat_drb <- ddat_wide %>%
    gather(site_no, discharge_cfs, -datetime) %>%
    mutate(site_id = sprintf('USGS-%s', site_no),
           discharge_cms = discharge_cfs / 35.314666) %>%
    select(-site_no, -discharge_cfs) %>%
    rename(date=datetime) %>%
    filter(!is.na(discharge_cms)) %>%
    filter(site_id %in% unique(drb_sites$site_id)) %>%
    left_join(distinct(drb_sites, site_id, seg_id_nat, subseg_id), by='site_id') %>%
    group_by(seg_id_nat, subseg_id, date) %>% 
    summarize(discharge_cms = mean(discharge_cms)) %>%
    ungroup()
  
  saveRDS(ddat_drb, as_data_file(out_ind))
  gd_put(out_ind)
}
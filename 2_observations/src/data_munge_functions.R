subset_sites <- function(out_ind, crosswalk_ind, dat_ind, fish_dist, bird_dist) {

  crosswalk <- readRDS(sc_retrieve(crosswalk_ind, 'getters.yml'))

  basin_sites <- crosswalk %>%
    mutate(bird_filtered = bird_dist_to_subseg_m < bird_dist,
      fish_filtered = abs(fish_dist_to_outlet_m) < fish_dist) %>%
    filter(bird_filtered, fish_filtered) %>%
    st_drop_geometry %>%
    distinct() # should check into why we need this

  # manual fix for a site that gets filtered out
  # need to adjust filter for upstream/downstream stream size
  add_back <- filter(crosswalk, site_id %in% 'USGS-01467059') %>%
    st_drop_geometry()

  basin_sites <- bind_rows(basin_sites, add_back)

  saveRDS(basin_sites, as_data_file(out_ind))
  gd_put(out_ind)

}

filter_temp_data <- function(cross_ind, dat_ind, ngwos_ind, out_ind) {

  sites <- readRDS(sc_retrieve(cross_ind, 'getters.yml')) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct(site_id, subseg_id, seg_id_nat, .keep_all = TRUE)

  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  ngwos_dat <- readRDS(sc_retrieve(ngwos_ind, 'getters.yml')) %>%
    mutate(site_id = paste0('USGS-', site_id)) %>%
    rename(temp_degC = temp_c)

  dat_all <- bind_rows(ungroup(dat), ungroup(ngwos_dat)) %>%
    mutate(source = gsub('nwiw', 'nwis', source))

  drb_dat <- filter(dat_all, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, temp_degC, .keep_all = TRUE)

  saveRDS(drb_dat, as_data_file(out_ind))
  gd_put(out_ind)

}

munge_temp_dat <- function(sites_ind, dat_ind, out_ind) {

  sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml')) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct()
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))

  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, temp_degC, .keep_all = TRUE) %>%
    group_by(site_id, date) %>%
    summarize(temp_C = mean(temp_degC)) %>%
    ungroup()

  drb_dat <- drb_dat %>%
    left_join(sites) %>%
    group_by(subseg_id, seg_id_nat, date) %>%
    summarize(temp_c = mean(temp_C),
              site_id = paste0(site_id, collapse = ', ')) %>%
    ungroup()

  saveRDS(drb_dat, as_data_file(out_ind))
  gd_put(out_ind)

}
generate_site_summary <- function(dat_ind, crosswalk_ind, out_ind) {
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml')) %>%
    group_by(site_id) %>%
    summarize(n_obs = n())

  delaware_pts <- dat %>%
    left_join(readRDS(sc_retrieve(crosswalk_ind, 'getters.yml'))) %>%
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
  ddat_wide <- readr::read_csv(sc_retrieve(dat_ind, 'getters.yml'),
                               col_types=cols(.default=col_double(), datetime=col_date(format='')))

  drb_sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml'))

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

summarize_temp <- function(in_ind, out_file) {
  dat <- readRDS(sc_retrieve(in_ind, 'getters.yml'))

  summary_post1980 <- dat %>%
    filter(date > as.Date('1980-10-01')) %>%
    filter(!is.na(seg_id_nat)) %>%
    group_by(seg_id_nat) %>%
    summarize(n_obs = n(), n_years = length(unique(lubridate::year(date))))

  summary_all <- dat %>%
    filter(!is.na(seg_id_nat)) %>%
    group_by(seg_id_nat) %>%
    summarize(n_obs = n(), n_years = length(unique(lubridate::year(date))))

  summary_total <- tibble(
    time_period = c('all', 'post-1980'),
    n_reaches_obs = c(nrow(summary_all), nrow(summary_post1980)),
    n_reaches_30yrs = c(sum(summary_all$n_years >=30), sum(summary_post1980$n_years >=30)),
    n_reaches_10k_dailies = c(sum(summary_all$n_obs >=10000), sum(summary_post1980$n_obs >= 10000)))

  write.csv(summary_total, out_file, row.names = FALSE)
}

# clean reservoir release data
clean_release_dat <- function(in_ind, out_ind, mgd_to_cfs) {

  dat <- readxl::read_xlsx(sc_retrieve(in_ind, 'getters.yml'), sheet = 'Sheet1', trim_ws = TRUE,)

  grand_ids <- tibble(reservoir = c('Neversink', 'Pepacton', 'Cannonsville'),
                      GRAND_ID = c(2200, 2192, 1550))
  dat_out <- dat %>%
    tidyr::pivot_longer(cols = -Date, names_to = 'release_type', values_to = 'release_volume') %>%
    mutate(reservoir = gsub('_.*', '', release_type),
           release_type = gsub('.*_', '', release_type)) %>%
    mutate(release_volume_cfs = release_volume*mgd_to_cfs,
           date = as.Date(Date)) %>%
    select(date, reservoir, release_type, release_volume_cfs) %>%
    left_join(grand_ids)

  readr::write_csv(x = dat_out, path = as_data_file(out_ind))
  gd_put(out_ind)

}

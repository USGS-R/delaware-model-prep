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

filter_temp_data <- function(cross_ind, dat_ind, out_ind) {

  sites <- readRDS(sc_retrieve(cross_ind, 'getters.yml')) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct(site_id, subseg_id, seg_id_nat, .keep_all = TRUE)

  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, mean_temp_degC, min_temp_degC, max_temp_degC, .keep_all = TRUE)

  saveRDS(drb_dat, as_data_file(out_ind))
  gd_put(out_ind)

}

mark_time_space_holdouts <- function(df, holdout_water_years, holdout_reach_ids){
  time_holdout_days <- sapply(holdout_water_years, water_year_to_days) %>%
    reduce(.f = c)
  df %>% mutate(in_time_holdout = date %in% time_holdout_days,
                in_space_holdout = seg_id_nat %in% holdout_reach_ids,
                test = in_time_holdout | in_space_holdout)
}

water_year_to_days <- function(year) {
  seq(from = as.POSIXct(paste0(year - 1, "-10-01"), tz = 'UTC'),
      to = as.POSIXct(paste0(year, "-09-30"), tz = 'UTC'), by="+1 day")
}

munge_split_temp_dat <- function(sites_ind, dat_ind, holdout_water_years,
                           holdout_reach_ids, out_ind) {

  sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml')) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct()
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))

  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, mean_temp_degC, .keep_all = TRUE) %>%
    group_by(site_id, date) %>%
    summarize(mean_temp_C = mean(mean_temp_degC),
              min_temp_C = min(min_temp_degC),
              max_temp_C = max(max_temp_degC)) %>%
    ungroup()

  drb_dat_by_subseg <- drb_dat %>%
    left_join(sites) %>%
    group_by(subseg_id, seg_id_nat, date) %>%
    summarize(mean_temp_c = mean(mean_temp_C),
              min_temp_c = min(min_temp_C),
              max_temp_c = max(max_temp_C),
              site_id = paste0(site_id, collapse = ', ')) %>%
    ungroup() %>%
    mark_time_space_holdouts(holdout_water_years, holdout_reach_ids)

  saveRDS(drb_dat_by_subseg, as_data_file(out_ind))
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



summarize_dat <- function(in_ind, out_file) {
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

  readr::write_csv(summary_total, out_file)
}

# clean reservoir release data
clean_release_dat <- function(in_ind, out_ind, mgd_to_cms) {

  dat <- readxl::read_xlsx(sc_retrieve(in_ind, 'getters.yml'), sheet = 'Sheet1', trim_ws = TRUE,)

  grand_ids <- tibble(reservoir = c('Neversink', 'Pepacton', 'Cannonsville'),
                      GRAND_ID = c(2200, 2192, 1550))
  dat_out <- dat %>%
    tidyr::pivot_longer(cols = -Date, names_to = 'release_type', values_to = 'release_volume') %>%
    mutate(reservoir = gsub('_.*', '', release_type),
           release_type = gsub('.*_', '', release_type)) %>%
    mutate(release_volume_cms = release_volume*mgd_to_cms,
           date = as.Date(Date)) %>%
    select(date, reservoir, release_type, release_volume_cms) %>%
    left_join(grand_ids)

  readr::write_csv(x = dat_out, path = as_data_file(out_ind))
  gd_put(out_ind)

}

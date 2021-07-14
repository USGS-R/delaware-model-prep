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
#' Function to retrieve all NWIS data from priority forecasting sites
#' Appends data from national pull (e.g., EcoSHEDS, WQP) when site-dates
#' are missing from NWIS
#'
#' @param out_ind output indicator file
#' @param sites priority sites from which to pull NWIS data
#' @param site_meta_ind indicator file of site metadata, which allows us to match USGS site number to seg_id_nat
#' @param pcode USGS parameter code
#' @param statcd USGS stat code
#' @param dummy_date A dummy date to modify if you want to trigger a rebuild
#' @param other_dat_ind Indicator file for other flow/temp data. This will be appended for missing NWIS sites/dates, so adds data from WQP or EcoSHEDS
#' @param holdout_water_years Test years to withhold from training
#' @param holdout_reach_ids Test sites to withhold from training
get_priority_data <- function(out_ind, sites, site_meta_ind, pcode, statcd,
                              dummy_date, other_dat_ind = NA, holdout_water_years,
                              holdout_reach_ids) {

  all_dat <- readNWISdv(siteNumbers = sites,
                        parameterCd = pcode,
                        statCd = statcd,
                        startDate = '1980-01-01') %>%
    renameNWISColumns()

  meta <- readRDS(sc_retrieve(site_meta_ind)) %>%
    filter(site_id %in% paste0('USGS-', sites)) %>%
    filter(source %in% 'nwis_dv') %>%
    select(site_id, seg_id_nat, subseg_id) %>% st_drop_geometry() %>% distinct()



  if (pcode %in% '00010') {
    out_dat <- all_dat %>%
      mutate(site_id = paste0('USGS-', site_no),
             date = Date) %>%
      select(site_id, date, mean_temp_c = Wtemp, min_temp_c = Wtemp_Min, max_temp_c = Wtemp_Max, cd = Wtemp_cd) %>%
      filter(!is.na(max_temp_c)) %>%
      left_join(meta)

    # site-dates we already have
    exclude <- paste0(out_dat$seg_id_nat, as.Date(out_dat$date))

    # bind data that doesn't come from nwis_dv
    other_dat <- readRDS(sc_retrieve(other_dat_ind)) %>%
      filter(seg_id_nat %in% meta$seg_id_nat) %>%
      mutate(compare = paste0(seg_id_nat, date)) %>%
      filter(!compare %in% exclude) %>%
      select(subseg_id, seg_id_nat, date, mean_temp_c, min_temp_c, max_temp_c, site_id)

    out <- bind_rows(out_dat, other_dat) %>%
      mutate(date = as.POSIXct(date))

  } else {
    out <- all_dat %>%
     mutate(discharge_cms = Flow / 35.314666,
            site_id = paste0('USGS-', site_no),
            date = as.POSIXct(Date, tz = 'UTC'),
            cd = Flow_cd) %>%
      select(site_id, date, discharge_cms, cd) %>%
      left_join(meta)
  }

  out <- out %>%
    ungroup() %>%
    filter(date >= as.POSIXct('1980-01-01', tz = 'UTC')) %>%
    mark_time_space_holdouts(holdout_water_years, holdout_reach_ids)

  readr::write_csv(out, as_data_file(out_ind))
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

# get most recent reservoir release data
get_releases <- function(out_ind, site_ids, reservoir_names) {
  dataRetrieval::setAccess('internal')
  sites <- data.frame(site_no = site_ids, reservoir = reservoir_names)
  dat <- dataRetrieval::readNWISuv(siteNumbers = site_ids,
                                   parameterCd = '00060',
                                   tz = "Etc/GMT+5") %>%
    mutate(releases_cms = X_.Spill.Release._00060_00000/35.314666) %>%
    mutate(date = as.Date(dateTime, tz = "Etc/GMT+5")) %>%
    group_by(site_no, date) %>%
    summarize(release_volume_cms = round(mean(releases_cms, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    left_join(sites)

  saveRDS(dat, file = as_data_file(out_ind))
  gd_put(out_ind)

}

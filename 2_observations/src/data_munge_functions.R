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

filter_temp_data_to_crosswalk <- function(cross_ind, dat_ind, out_ind) {

  sites <- readRDS(sc_retrieve(cross_ind, 'getters.yml')) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct(site_id, subseg_id, seg_id_nat, .keep_all = TRUE)

  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, mean_temp_degC, min_temp_degC, max_temp_degC, .keep_all = TRUE) %>%
    mutate(mean_temp_degC = round(mean_temp_degC, 1),
           min_temp_degC = round(min_temp_degC, 1),
           max_temp_degC = round(max_temp_degC, 1))

  saveRDS(drb_dat, as_data_file(out_ind))
  gd_put(out_ind)

}

filter_temp_data_to_sites <- function(sites_ind, dat_ind, out_ind){

  sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml')) %>%
    select(site_id, subseg_id, seg_id_nat) %>%
    distinct()

  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))

  drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
    distinct(site_id, date, mean_temp_degC, .keep_all = TRUE) %>%
    left_join(sites, by = "site_id") %>%
    mutate(date = as.Date(date))

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


#' @param prioritize_nwis_sites logical; indicates whether segment summaries should
#' prioritize NWIS observations when available. Defaults to TRUE. If TRUE, any
#' observations from non-NWIS sites will be omitted from the summarized data.
munge_split_temp_dat <- function(dat_ind,
                                 holdout_water_years,
                                 holdout_reach_ids,
                                 out_ind,
                                 prioritize_nwis_sites = TRUE) {

  drb_dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  # first find sites that have the same site_id but have a sub-location
  # this is restricted to NWIS sites that returned multiple columns per site
  # can be multiple sensors (concurrently measuring at different locations), or could represent
  # different sensors through time. Some sensors we do not want to keep (piezometers)

  # first handle multiple sub_locations
  # that are causing >1 obs per site_id-date
  # slice_max on n_sub selects the sub-location
  # with the most observations
  sub_location_res <- drb_dat %>%
    filter(!grepl('piezometer', sub_location, ignore.case = TRUE)) %>%
    filter(!is.na(sub_location)) %>%
    group_by(site_id, sub_location) %>%
    mutate(n_sub = n()) %>% ungroup() %>%
    group_by(site_id, date) %>%
    slice_max(order_by = n_sub, n = 1, with_ties = FALSE) %>% ungroup()

  # bind back with data without sub_location data
  # resolve remaining site_ids with mutliple obs

  drb_dat2 <- bind_rows(sub_location_res,
                        filter(drb_dat, is.na(sub_location)))

  # resolve remaining site_ids with >1 obs per date
  # take the site_id with the most n_obs

  drb_dat_dup_resolved <- drb_dat2 %>%
    group_by(site_id, date) %>%
    slice_max(order_by = n_obs, n = 1, with_ties = FALSE) %>%
    ungroup()

  drb_dat_by_subseg <- drb_dat_dup_resolved %>%
    group_by(subseg_id, seg_id_nat, date) %>%
    # If prioritize_nwis_sites is TRUE, check whether data for that segment
    # comes from multiple sources. If multiple distinct site id's, retain only
    # NWIS sites for that segment-date; otherwise, retain all samples
    {if(prioritize_nwis_sites){
      filter(., if(n_distinct(site_id) > 1 & any(grepl("nwis", source, ignore.case = TRUE))) grepl("nwis", source, ignore.case = TRUE) else TRUE)
    } else {.}
    } %>%
    summarize(site_id = paste0(site_id, collapse = ', '),
              source = paste0(unique(source), collapse = ', '),
              time = ifelse(n() > 1, NA, time), # keep timestamp if represents a single value
              mean_temp_c = round(mean(mean_temp_degC), 1),
              min_temp_c = min(min_temp_degC),
              max_temp_c = max(max_temp_degC),
              sd_mean_temp_c = round(sd(mean_temp_degC), 1), # provide an indicator of variability across sites
              flag = paste(unique(flag)[!is.na(unique(flag))], collapse = '; '),
              .groups = 'drop') %>%
    rowwise() %>%
    mutate(flag = paste(unique(unlist(strsplit(flag, '; '))), collapse = '; ')) %>%
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
      select(site_id, date, mean_temp_c = Wtemp, min_temp_c = Wtemp_Min, max_temp_c = Wtemp_Max) %>%
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
     mutate(discharge_cms = round(Flow / 35.314666, 3),
            site_id = paste0('USGS-', site_no),
            date = as.POSIXct(Date, tz = 'UTC'),
            cd = Flow_cd) %>%
      select(site_id, date, discharge_cms) %>%
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

  dat <- readxl::read_xlsx(sc_retrieve(in_ind, 'getters.yml'),
                           sheet = 'Sheet1',
                           trim_ws = TRUE, skip = 1, col_names = c('Date', 'Neversink_Conser', 'Neversink_Direct', 'Pepacton_Conser', 'Pepacton_Direct', 'Cannonsville_Conser', 'Cannonsville_Direct', 'Neversink_Spill', 'Pepacton_Spill', 'Cannonsville_Spill'))

  grand_ids <- tibble(reservoir = c('Neversink', 'Pepacton', 'Cannonsville'),
                      GRAND_ID = c(2200, 2192, 1550))
  dat_out <- dat %>%
    tidyr::pivot_longer(cols = -Date, names_to = 'release_type', values_to = 'release_volume') %>%
    mutate(reservoir = gsub('_.*', '', release_type),
           release_type = gsub('.*_', '', release_type)) %>%
    mutate(release_volume_cms = round(release_volume*mgd_to_cms, 3),
           date = as.Date(Date)) %>%
    select(date, reservoir, release_type, release_volume_cms) %>%
    left_join(grand_ids)

  readr::write_csv(x = dat_out, file = as_data_file(out_ind))
  gd_put(out_ind)

}

# get most recent reservoir release data
get_releases <- function(out_ind, site_ids, reservoir_names, cfs_to_cms = 0.0283) {

  dataRetrieval::setAccess('internal')
  sites <- data.frame(site_no = site_ids, reservoir = reservoir_names)


  # in DV, all three reservoirs have columns that represent spillway releases,
  # controlled releases, and diversions. The numbers are returned as numbered columns,
  # and all three have different orders that you can't ascertain from the naming convention
  # see small text under figures for order:
  # Cannonsville:https://nwis.waterdata.usgs.gov/ny/nwis/dv/?site_no=01436499&agency_cd=USGS&amp;referred_module=sw
  # Pepacton:https://nwis.waterdata.usgs.gov/ny/nwis/dv?cb_00060=on&cb_00060=on&cb_00060=on&cb_72022=on&format=gif_default&site_no=01417499&referred_module=sw&period=&begin_date=2020-03-18&end_date=2022-03-23
  # Neversink:https://waterdata.usgs.gov/nwis/dv/?site_no=01436599&agency_cd=USGS&amp;referred_module=sw
  dat_neversink <- dataRetrieval::readNWISdv(siteNumbers = sites$site_no[sites$reservoir %in% 'Neversink'],
                                   parameterCd = '00060') %>%
    mutate(
      releases_cms = cfs_to_cms*`X_ODRM.Computation_00060_00003`,
      diversions_cms = cfs_to_cms*`X_ODRM.Computation....2.._00060_00003`,
      spillway_cms = cfs_to_cms*`X_ODRM.Computation....3.._00060_00003`) %>%
    select(site_no, Date, releases_cms, diversions_cms, spillway_cms)

  dat_cannonsville <- dataRetrieval::readNWISdv(siteNumbers = sites$site_no[sites$reservoir %in% 'Cannonsville'],
                                                parameterCd = '00060') %>%
    mutate(
      spillway_cms = cfs_to_cms*`X_ODRM.Computations_00060_00003`,
      releases_cms = cfs_to_cms*`X_ODRM.Computations....2.._00060_00003`,
      diversions_cms = cfs_to_cms*`X_ODRM.Computations....3.._00060_00003`) %>%
    select(site_no, Date, releases_cms, diversions_cms, spillway_cms)

  dat_pepacton <- dataRetrieval::readNWISdv(siteNumbers = sites$site_no[sites$reservoir %in% 'Pepacton'],
                                                parameterCd = '00060') %>%
    mutate(
      diversions_cms = cfs_to_cms*`X_ODRM.Computations_00060_00003`,
      spillway_cms = cfs_to_cms*`X_ODRM.Computations....2.._00060_00003`,
      releases_cms = cfs_to_cms*`X_ODRM.Computations....3.._00060_00003`) %>%
    select(site_no, Date, releases_cms, diversions_cms, spillway_cms)

  out <- bind_rows(dat_cannonsville, dat_pepacton, dat_neversink) %>%
    mutate(total_releases_cms = releases_cms + spillway_cms) %>%
    rename(date = Date) %>%
    left_join(sites) %>%
    select(-site_no)

  saveRDS(out, file = as_data_file(out_ind))
  gd_put(out_ind)

}

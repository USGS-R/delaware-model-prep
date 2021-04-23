library(scipiper)

scmake('2_observations/out/obs_temp_drb.rds', remake_file = 'getters.yml')
scmake('2_observations/out/obs_flow_drb.rds', remake_file = 'getters.yml')
scmake('1_network/out/segments_relative_to_reservoirs.rds', remake_file = 'getters.yml')
scmake('1_network/out/subseg_distance_matrix.rds', remake_file = 'getters.yml')

temp_obs <- readRDS('2_observations/out/obs_temp_drb.rds')
flow_obs <- readRDS('2_observations/out/obs_flow_drb.rds') %>%
  mutate(date = as.POSIXct(date, tz = 'UTC'))
reservoir_segs <- readRDS('1_network/out/segments_relative_to_reservoirs.rds')
distance_matrix <- readRDS('1_network/out/subseg_distance_matrix.rds')
network <- readRDS('1_network/out/network.rds')

#created in Jeff's Snakefile; I downloaded this file from Caldera
catchment_attributes <- feather::read_feather('seg_attr_drb.feather')
catchment_att_metadata <- readr::read_csv('combined_metadata.csv')

#could put these in a yaml?  Or in a single list object that would be more easily re-used
append_key_seg_names <- function(df) {
  neversink_seg_ids <- c('1645', '1638')
  beltzville_seg_ids <- c('1703', '1697')
  christina_seg_ids <- c('2007', '2012', '2013', '2014', '2037')
  df %>%
    mutate(key_seg = case_when(seg_id_nat %in% neversink_seg_ids ~ 'Neversink reservoir',
                               seg_id_nat %in% beltzville_seg_ids ~ 'Beltzville reservoir',
                               seg_id_nat %in% christina_seg_ids ~ 'Christina basin',
                               TRUE ~ NA_character_))
}


library(tidyverse)
##### distance matrix #####
#transform distance matrix to table of distances between reaches with data, and
#table of distances between reaches with data and reservoirs
temp_seg_ids <- unique(temp_obs$subseg_id)
updown_matrix_monitored <- distance_matrix$updown[temp_seg_ids, temp_seg_ids]
#minimum of each row
min_abs_not_zero <- function(x) {
  no_zeros <- x[x != 0]
  no_zeros[which.min(abs(no_zeros))]
}
row_mins <- apply(updown_matrix_monitored, 1, min_abs_not_zero)

#could check Inf reaches here; should be on isolated segments near the bay?
#filter out of row_mins, join to geom, plot with that as color
inf_reaches <- row_mins[is.infinite(row_mins)]

#get headwater reaches: upstream matrix, rows with no real values  not 0
all_inf_or_zero <- function(x) {
  all(x == 0 | is.infinite(x))
}
headwater_reaches <- rownames(distance_matrix$upstream)[apply(distance_matrix$upstream, MARGIN = 1, FUN = all_inf_or_zero)]

#reach distance upstream to reservoir-containing or overlapping reach
#get site names, check others that shouldn't be held out

##### Look for freebies — only data during test time periods #####
#plot map with color by fraction training?

year_to_days <- function(year) {
  seq(from = as.POSIXct(paste0(year, "-01-01"), tz = 'UTC'),
      to = as.POSIXct(paste0(year, "-12-31"), tz = 'UTC'), by="+1 day")
}

summarize_holdout <- function(df) {
  df %>%
    group_by(seg_id_nat) %>%
    summarize(fraction_heldout = sum(in_time_holdout) / n(),
              n_data_points = n())
}





time_holdout_years <- c(1980:1984, 2011:2015, 2021)
time_holdout_days <- sapply(time_holdout_years, year_to_days) %>%
  reduce(.f = c)

temp_obs_time_holdout <- temp_obs %>%
  mutate(in_time_holdout = date %in% time_holdout_days)

flow_obs_time_holdout <- flow_obs %>%
  mutate(in_time_holdout = date %in% time_holdout_days)

#TODO: join on other attributes so see which these cover
#TODO: getting 2021 data soon?  important here for holdouts
temp_obs_time_holdout_summary <- temp_obs_time_holdout %>%
  summarize_holdout()

flow_obs_time_holdout_summary <- flow_obs_time_holdout %>%
  summarize_holdout() %>%


flow_obs_stats <- flow_obs %>%
  group_by(seg_id_nat) %>%
  summarize(n = n(),
            median_daily_q = median(discharge_cms),
            mean_daily_q = mean(discharge_cms),
            min_daily_q = min(discharge_cms),
            max_daily_q = max(discharge_cms),
            iqr_daily_q = IQR(discharge_cms))

all_info <- temp_obs_time_holdout_summary %>%
  left_join(flow_obs_time_holdout_summary, by = 'seg_id_nat',
            suffix = c('_temp', '_flow')) %>%
  left_join(flow_obs_stats, by = 'seg_id_nat') %>%
  append_key_seg_names() %>%
  left_join(reservoir_segs, by = 'seg_id_nat')

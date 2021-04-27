library(scipiper)
source('test_train_split/src/functions.R')
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

#created in Jeff's Snakefile in this repo (20_catchment_attributes); I downloaded this file from Caldera
catchment_attributes <- feather::read_feather('seg_attr_drb.feather')
catchment_att_metadata <- readr::read_csv('combined_metadata.csv')

library(tidyverse)

##### distance matrix #####
#transform distance matrix to table of distances between reaches with data, and
#table of distances between reaches with data and reservoirs
temp_seg_ids <- unique(temp_obs$subseg_id)
updown_matrix_monitored <- distance_matrix$updown[temp_seg_ids, temp_seg_ids]

reach_mouths_monitored <- network$vertices %>%
  mutate(ends_subseg = na_if(ends_subseg, '')) %>%
  select(-point_ids, -starts_subseg) %>%
  separate_rows(ends_subseg, sep = ';') %>%
  filter(!is.na(ends_subseg),
         ends_subseg %in% temp_seg_ids)
#use greps with subseg_ids to match with this
reach_mouth_bird_matrix <- st_distance(reach_mouths_monitored) %>% units::drop_units()
rownames(reach_mouth_bird_matrix) <- reach_mouths_monitored$ends_subseg
colnames(reach_mouth_bird_matrix) <- reach_mouths_monitored$ends_subseg

#could check Inf reaches here; should be on isolated segments near the bay or very close to mouth
#filter out of row_mins, join to geom, plot with that as color
# inf_reaches <- sites_dist_to_nearest_site[is.infinite(sites_dist_to_nearest_site)]
# network_inf <- network$edges %>%
#   mutate(inf_reach = subseg_id %in% names(inf_reaches))
# ggplot(network_inf) + geom_sf(aes(color = inf_reach)) + theme_minimal()


#get headwater reaches: upstream matrix, rows with no real values  not 0
headwater_reaches <- rownames(distance_matrix$upstream)[apply(distance_matrix$upstream, MARGIN = 1, FUN = all_inf_or_zero)]
network_headwater <- network$edges %>%
  mutate(headwater_reach = subseg_id %in% headwater_reaches)
ggplot(network_headwater) + geom_sf(aes(color = headwater_reach)) + theme_minimal()


#reach distance upstream to reservoir-containing or overlapping reach
reservoir_overlapping_reaches <- filter(reservoir_segs, grepl(pattern = 'inlet|outlet|contain|within', x = type_res)) %>%
  left_join(select(network$edges, seg_id_nat, subseg_id), by = 'seg_id_nat')
up_matrix_monitor_to_reservoir <- distance_matrix$upstream[temp_seg_ids,reservoir_overlapping_reaches$subseg_id]
temp_to_reservoir_row_mins <- apply(up_matrix_monitor_to_reservoir, 1, min_abs_not_zero) %>%
  na_if(Inf) %>%
  tibble(dist_up_to_reservoir = ., subseg_id = names(.))

#get site names, check others that shouldn't be held out
temp_site_ids <- temp_obs %>% select(site_id) %>%
  distinct() %>%
  separate_rows(site_id, sep = ',') %>%
  distinct() %>%
  filter(grepl(pattern = '^USGS-', x = site_id)) %>%
  mutate(site_no = gsub('USGS-', replacement = '', x = site_id))
temp_site_info <- dataRetrieval::readNWISdata(service = 'site',
                                              sites = temp_site_ids$site_no,
                                              siteOutput = 'expanded') %>%
  left_join(temp_site_ids, by = 'site_no')


##### Look for freebies — only data during test time periods #####
#plot map with color by fraction training?

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
  summarize_holdout()


flow_obs_stats <- flow_obs %>%
  group_by(seg_id_nat, subseg_id) %>%
  summarize(n = n(),
            median_daily_q = median(discharge_cms),
            mean_daily_q = mean(discharge_cms),
            min_daily_q = min(discharge_cms),
            max_daily_q = max(discharge_cms),
            iqr_daily_q = IQR(discharge_cms))
temp_obs_stats <- temp_obs %>%
  group_by(seg_id_nat, subseg_id) %>%
  summarize(n = n())

all_obs_stats <- full_join(temp_obs_stats, flow_obs_stats, by = c('seg_id_nat', 'subseg_id'),
                           suffix = c('_temp', '_flow'))

#bird distance as well as fish distance
#obs count for closest observed reach, or possibly obs within X fish or bird distance?  How variable is reach length?
fish_dist <- apply(updown_matrix_monitored, 1, get_distance_metrics, radius = 10e3,
                   obs_summary = all_obs_stats, col_name_suffix = 'fish') %>%
  bind_rows(.id = 'subseg_id')

bird_dist <- apply(reach_mouth_bird_matrix, 1, get_distance_metrics, radius = 10e3,
                   obs_summary = all_obs_stats, col_name_suffix = 'bird') %>%
  bind_rows(.id = 'subseg_id')

#TODO: headwaters, distance up to reservoir, distance to nearest site, catchment info
all_info <- temp_obs_time_holdout_summary %>%
  left_join(flow_obs_time_holdout_summary, by = 'seg_id_nat',
            suffix = c('_temp', '_flow')) %>%
  left_join(flow_obs_stats, by = c('seg_id_nat')) %>%
  append_key_seg_names() %>%
  left_join(reservoir_segs, by = 'seg_id_nat') %>%
  mutate(subseg_seg = as.numeric(subseg_seg)) %>%
  left_join(network$edges, by = c('seg_id_nat', 'subseg_seg', 'subseg_id')) %>%
  mutate(headwater = subseg_id %in% headwater_reaches) %>%
  left_join(temp_to_reservoir_row_mins, by = 'subseg_id') %>%
  left_join(fish_dist, by = 'subseg_id') %>%
  left_join(bird_dist, by = 'subseg_id')
  left_join(catchment_attributes, by = 'seg_id_nat')


all_info_gt400 <- all_info %>% filter(n_data_points_temp > 400)
ggplot(all_info_gt400) + geom_sf(aes(color = fraction_heldout_temp, geometry = geometry)) + theme_minimal() +
  scale_color_gradient(low = 'white', high = 'red')
ggplot(all_info, aes(x = dist_to_nearest_site)) + geom_histogram()

ggplot(network$edges, aes(x = as.numeric(subseg_length))) + stat_ecdf() +
  scale_x_continuous(labels = scales::comma) + labs(x = 'Reach length (m)')

#sanity check: n_obs_xxx_radius_fish should never exceed n_obs_xxx_radius_bird (fish radius can never be shorter than bird radius)
ggplot(all_info, aes(x = n_obs_temp_radius_fish, y = n_obs_temp_radius_bird)) + geom_abline(slope = 1) + geom_point()

#TODO: distribution of data close by; check sites of interest
ggplot(all_info_gt400, aes(x = n_obs_temp_radius_fish)) + stat_ecdf() + scale_x_log10()
ggplot(all_info_gt400, aes(x = n_temp_closest_fish)) + stat_ecdf() + scale_x_log10()

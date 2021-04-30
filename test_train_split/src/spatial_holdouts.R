library(scipiper)
source('test_train_split/src/functions.R')
scmake('2_observations/out/obs_temp_drb.rds', remake_file = 'getters.yml')
scmake('2_observations/out/obs_flow_drb.rds', remake_file = 'getters.yml')
scmake('1_network/out/segments_relative_to_reservoirs.rds', remake_file = 'getters.yml')
scmake('1_network/out/subseg_distance_matrix.rds', remake_file = 'getters.yml')

min_date <- '1980-01-01'

temp_obs <- readRDS('2_observations/out/obs_temp_drb.rds') %>%
  filter(date > min_date)
flow_obs <- readRDS('2_observations/out/obs_flow_drb.rds') %>%
  mutate(date = as.POSIXct(date, tz = 'UTC')) %>%
  filter(date > min_date)
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
obs_search_radius <- 20e3
fish_dist <- apply(updown_matrix_monitored, 1, get_distance_metrics, radius = obs_search_radius,
                   obs_summary = all_obs_stats, col_name_suffix = 'fish') %>%
  bind_rows(.id = 'subseg_id')

bird_dist <- apply(reach_mouth_bird_matrix, 1, get_distance_metrics, radius = obs_search_radius,
                   obs_summary = all_obs_stats, col_name_suffix = 'bird') %>%
  bind_rows(.id = 'subseg_id')

#TODO: headwaters, distance up to reservoir, distance to nearest site, catchment info
all_info <- temp_obs_time_holdout_summary %>%
  left_join(flow_obs_time_holdout_summary, by = c('seg_id_nat', 'subseg_id'),
            suffix = c('_temp', '_flow')) %>%
  left_join(flow_obs_stats, by = c('seg_id_nat', 'subseg_id')) %>%
  append_key_seg_names() %>%
  left_join(reservoir_segs, by = 'seg_id_nat') %>%
  mutate(subseg_seg = as.numeric(subseg_seg)) %>%
  left_join(network$edges, by = c('seg_id_nat', 'subseg_seg', 'subseg_id')) %>%
  mutate(headwater = subseg_id %in% headwater_reaches) %>%
  left_join(temp_to_reservoir_row_mins, by = 'subseg_id') %>%
  left_join(fish_dist, by = 'subseg_id') %>%
  left_join(bird_dist, by = 'subseg_id') %>%
  left_join(catchment_attributes, by = 'seg_id_nat')


all_info_gt400 <- all_info %>% filter(n_data_points_temp > 400)
ggplot(all_info_gt400) + geom_sf(aes(color = fraction_heldout_temp, geometry = geometry)) + theme_minimal() +
  scale_color_gradient(low = 'white', high = 'red')

ggplot(network$edges, aes(x = as.numeric(subseg_length))) + stat_ecdf() +
  scale_x_continuous(labels = scales::comma) +
  labs(x = 'Reach length (m)', y = 'Empirical cumulative distribution function')

#sanity check: n_obs_xxx_radius_fish should never exceed n_obs_xxx_radius_bird (fish radius can never be shorter than bird radius)
ggplot(all_info, aes(x = n_obs_temp_radius_fish, y = n_obs_temp_radius_bird)) + geom_abline(slope = 1) + geom_point()

#TODO: distribution of data close by; check sites of interest
ggplot(all_info_gt400, aes(x = n_obs_temp_radius_fish)) + stat_ecdf() + scale_x_log10()
ggplot(all_info_gt400, aes(x = n_temp_closest_fish)) + stat_ecdf() + scale_x_log10()

key_segments <- get_key_segments()
all_info_select <- all_info_gt400 %>%
  filter(seg_id_nat %in% key_segments$lordville_id |
         seg_id_nat == '2007' | #headwater reach in Christina sub-basin
         seg_id_nat == key_segments$montague_seg_id |
         seg_id_nat == key_segments$trenton_seg_id |
         seg_id_nat %in% key_segments$neversink_seg_ids |
         seg_id_nat %in% key_segments$beltzville_seg_ids |
          seg_id_nat %in% c('2319', '3570'))
ggplot(all_info_gt400, aes(x = n_obs_temp_radius_fish)) + stat_ecdf() + scale_x_log10() +
  geom_vline(data = all_info_select, aes(xintercept = n_obs_temp_radius_fish, color = key_seg), lwd = 1,
             alpha = 1) +
  labs(x = sprintf('Temperature observations within %s m of reach', obs_search_radius),
       y = 'Cumulative distribution function')

ggplot(all_info_gt400, aes(x = n_obs_flow_radius_fish)) + stat_ecdf() + scale_x_log10() +
  geom_vline(data = all_info_select, aes(xintercept = n_obs_flow_radius_fish, color = key_seg))

ggplot(network$edges) + geom_sf() + theme_minimal() +
  geom_sf(data = all_info_select, aes(color = key_seg, geometry = geometry), lwd = 3)


all_info_lt100_radius <- all_info %>%
  filter(n_obs_temp_radius_fish < 100)

#reaches with >400 temp obs, less than 100 within radius
ggplot(network$edges) + geom_sf() + theme_minimal() +
  geom_sf(data = all_info_lt100_radius, aes(color = n_data_points_temp, geometry = geometry), lwd = 3) +
  scale_color_binned()

#All delaware mainstem monitored reaches
delaware_reaches <- get_delaware_mainstem_sites() %>%
  group_by(subseg_id) %>%
  summarize(station_names = paste(station_nm, collapse = '|'))
all_info_delaware_mainstem <- all_info_gt400 %>%
  filter(grepl(pattern = 'delaware', x = key_seg, ignore.case = TRUE)) %>%
  left_join(delaware_reaches, by = c('subseg_id'))

ggplot(network$edges) + geom_sf() + theme_minimal() +
   geom_sf(data = all_info_delaware_mainstem, aes(color = n_data_points_temp, geometry = geometry), lwd = 3) +
   #geom_sf_label(data = all_info_delaware_mainstem, aes(label = subseg_id, geometry = geometry)) +
   ggrepel::geom_label_repel(
    data = all_info_delaware_mainstem,
    aes(label = subseg_id, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "magenta",
    segment.colour = "magenta"
  )
  # geom_sf(data = filter(all_info_delaware_mainstem, key_seg != 'Delaware mainstem'),
  #         aes(color = key_seg, geometry = geometry), inherit.aes = FALSE)

ggplot(all_info_delaware_mainstem, aes(x = n_data_points_temp, y = n_obs_temp_radius_fish, label = subseg_id)) + geom_point() +
  ggrepel::geom_label_repel() +
  labs(x = 'Number of temperature observations', y = sprintf('Temperature observations within %s m of reach', obs_search_radius))


#network mouths?
network_mouths <- network$edges %>% st_line_sample(sample = 1)
network_coords <- network_mouths %>% st_coordinates() %>%
  as_tibble() %>% bind_cols(select(network$edges, subseg_id))
network_coords_north_south <- network_coords %>%
  arrange(desc(Y)) %>%
  mutate(order = 1:n())

#sanity check reach mouths
ggplot(network$edges) + geom_sf() + theme_minimal() +
  geom_sf(data = network_mouths, color = 'red')
reach_time_range_plot(subseg_ids = all_info_delaware_mainstem$subseg_id,
                      obs_df = temp_obs_time_holdout,
                      min_year = 1950,
                      holdout_years = time_holdout_years,
                      subseg_order_df = network_coords_north_south,
                      title = 'Mainstem reaches ordered N -> S')

reach_time_range_plot(all_info_select$subseg_id,
                      obs_df = temp_obs_time_holdout,
                      min_year = 1950,
                      holdout_years = time_holdout_years,
                      subseg_order_df = tibble(subseg_id = all_info_select$subseg_id,
                                               order = 1:nrow(all_info_select)),
                      title = 'Select reaches of interest')

ggplot(network$edges) + geom_sf() + theme_minimal() +
  geom_sf(data = all_info_select, aes(color = n_data_points_temp, geometry = geometry), lwd = 3) +
  #geom_sf_label(data = all_info_delaware_mainstem, aes(label = subseg_id, geometry = geometry)) +
  ggrepel::geom_label_repel(
    data = all_info_select,
    aes(label = subseg_id, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "magenta",
    segment.colour = "magenta"
  ) +
  labs(title = 'Reaches of interest')

#what fraction of total data at each site?
all_obs_stats_frac_total <- all_obs_stats %>%
  ungroup() %>%
  mutate(frac_obs_temp = n_temp / sum(n_temp, na.rm = TRUE),
         frac_obs_flow = n_flow / sum(n_flow, na.rm = TRUE),
         label = if_else(frac_obs_temp > 0.005, true = subseg_id, false = NA_character_)) %>%
  replace_na(replace = list(frac_obs_temp = 0, frac_obs_flow = 0))
ggplot(all_obs_stats_frac_total, aes(x = frac_obs_temp*100, y = 100*frac_obs_flow)) + geom_point() +
  ggrepel::geom_text_repel(aes(label = label), max.overlaps = 20) +
  labs(x = 'Percent of total temperature observation days', y = 'Percent of total flow observation days',
       title = 'Percent of total observation days at each reach')

all_obs_stats_frac_added_by_spatial <- temp_obs_time_holdout %>%
  mutate(has_temp = TRUE) %>%
  full_join(flow_obs_time_holdout, by = c('subseg_id', 'seg_id_nat', 'date', 'in_time_holdout')) %>%
  group_by(subseg_id, in_time_holdout) %>%
  summarize(n_days_temp = sum(!is.na(has_temp)), #join will fill in NAs where only flow or temp values
            n_days_flow = sum(!is.na(discharge_cms))) %>%
  mutate(fraction_total_temp = n_days_temp / sum(all_obs_stats$n_temp, na.rm = TRUE),
         fraction_total_flow = n_days_flow / sum(all_obs_stats$n_flow, na.rm = TRUE))


#what is the test/train split based solely on time?
all_obs_stats_frac_added_by_spatial %>% group_by(in_time_holdout) %>%
  summarize(total_frac_temp = sum(fraction_total_temp),
            total_frac_flow = sum(fraction_total_flow))

#now what does each site contribute on top of that if it is a spatial holdout?
all_obs_stats_frac_outside_time_holdout <- all_obs_stats_frac_added_by_spatial %>%
  filter(!in_time_holdout) %>%
  mutate(label = if_else(fraction_total_temp > 0.005, true = subseg_id, false = NA_character_))
ggplot(all_obs_stats_frac_outside_time_holdout, aes(x = 100*fraction_total_temp, y = 100*fraction_total_flow)) +
  geom_point() + ggrepel::geom_text_repel(aes(label = label)) +
  labs(title = 'Additional loss of training data if a reach is a spatial holdout',
       x = 'Percent temp data lost', y = 'Percent flow data lost')

#check urban drainages impervious > 0.2
all_info_gt400_imperv <- filter(all_info_gt400, hru_percent_imperv > 0.2)
ggplot(network$edges) + geom_sf() + theme_minimal() +
  geom_sf(data = all_info_gt400_imperv, aes(color = n_data_points_temp, geometry = geometry), lwd = 3) +
  #geom_sf_label(data = all_info_delaware_mainstem, aes(label = subseg_id, geometry = geometry)) +
  ggrepel::geom_label_repel(
    data = all_info_gt400_imperv,
    aes(label = subseg_id, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    colour = "magenta",
    segment.colour = "magenta",
    max.overlaps = 15
  ) +
  labs(title = 'Reaches with >400 temp obs, >0.2 imperv')
#fish radius vs % imperv
ggplot(all_info_gt400_imperv, aes(x = hru_percent_imperv, y = n_obs_temp_radius_fish, color = n_data_points_temp)) +
  geom_point() + ggrepel::geom_text_repel(aes(label = subseg_id)) +
  labs(title = 'Reaches with at least 400 temp obs, >0.2 impervious')
#data time ranges in imperv subsegs
reach_time_range_plot(all_info_gt400_imperv$subseg_id,
                      obs_df = temp_obs_time_holdout,
                      min_year = 1980,
                      holdout_years = time_holdout_years,
                      subseg_order_df = tibble(subseg_id = all_info_select$subseg_id,
                                               order = 1:nrow(all_info_select)),
                      title = 'Reaches % impervious > 0.2')

##### specify holdouts #####
holdout_segs <- tibble(seg_id_nat = as.numeric(c(key_segments$beltzville_seg_ids, '2007',
                                      key_segments$lordville_id, key_segments$trenton_seg_id,
                                      '3570', '2338'))) %>%
  left_join(select(all_info, seg_id_nat, subseg_id, key_seg), by = 'seg_id_nat')

#add up data not in time holdout, get total fraction added by holding out these sites
spatial_holdout_frac_time_removed <- all_obs_stats_frac_added_by_spatial %>%
  filter(!in_time_holdout) %>%
  right_join(holdout_segs, by = c('subseg_id'))
sum(spatial_holdout_frac_time_removed$fraction_total_temp)
sum(spatial_holdout_frac_time_removed$fraction_total_flow)

#by mean discharge, basin characteristics
#dotplot highlighting held-out reaches
flow_catchment_atts <- flow_obs_stats %>%
  left_join(catchment_attributes, by = c('seg_id_nat')) %>%
  mutate(holdout = subseg_id %in% holdout_segs$subseg_id)

flow_catchment_atts_long <- flow_catchment_atts %>%
  pivot_longer(cols = !all_of(c('seg_id_nat', 'subseg_id', 'n', 'holdout')),
               names_to = 'reach_metric')
ggplot(flow_catchment_atts_long, aes(x = value, fill = holdout)) +
  geom_dotplot() +
  facet_wrap('reach_metric', scales = 'free') +
  scale_y_continuous(NULL, breaks = NULL)
ggplot(flow_catchment_atts, aes(x = cov_type, fill = holdout)) + geom_dotplot()

#for final output, add columns to observations for time, spatial, either holdout
temp_obs_marked_holdout <- temp_obs_time_holdout %>%
  mutate(in_spatial_holdout = subseg_id %in% holdout_segs$subseg_id,
         in_any_holdout = in_time_holdout | in_spatial_holdout)
saveRDS(temp_obs_marked_holdout, file = 'test_train_split/out/temp_obs_marked_holdout.rds')

flow_obs_marked_holdout <- flow_obs_time_holdout %>%
  mutate(in_spatial_holdout = subseg_id %in% holdout_segs$subseg_id,
         in_any_holdout = in_time_holdout | in_spatial_holdout)
saveRDS(temp_obs_marked_holdout, file = 'test_train_split/out/flow_obs_marked_holdout.rds')

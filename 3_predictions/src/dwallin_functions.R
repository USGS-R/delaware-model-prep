calc_decay <- function(segs_downstream,
                       reservoir_nhdid,
                       preds_obs_ind = '3_predictions/out/compare_predictions_obs.feather.ind',
                       glm_preds_file,
                       res_data = '../lake-temperature-model-prep/7b_temp_merge/out/drb_daily_reservoir_temps.rds',
                       test_period,
                       distance_file = '1_network/out/subseg_distance_matrix.rds',
                       crosswalk_ind = '2_observations/out/crosswalk_site_reach.rds.ind',
                       network_file = '1_network/out/network.rds',
                       reservoir_subseg) {

  segs_keep <- segs_downstream

  # get preds/obs
  compare <- feather::read_feather(sc_retrieve(preds_obs_ind)) %>%
    mutate(date = as.Date(date)) %>%
    filter(seg_id_nat %in% segs_keep) %>%
    filter(!is.na(mean_temp_c)) %>%
    mutate(doy = lubridate::yday(date),
           year = lubridate::year(date))

  # stream network
  network <- readRDS(network_file)[[1]]

  # crosswalk between segs and sites
  cross <- readRDS(sc_retrieve(crosswalk_ind))

  # right now this is deep-water reservoir observations
  # but we will switch this to GLM outflow predictions
  res <- readRDS(res_data) %>%
    filter(site_id %in% reservoir_nhdid) %>%
    mutate(year = lubridate::year(date),
           doy = lubridate::yday(date)) %>%
    group_by(date) %>%
    slice_max(depth) %>%
    filter(depth > 30)

  # stream dat
  stream_dat <- compare

  unique_segs <- unique(stream_dat$seg_id_nat)

  # distance matrix to figure out weighting scheme
  dist <- readRDS(distance_file)[[2]]

  distances <- tibble(subseg_id = names(dist[reservoir_subseg,network$subseg_id[network$seg_id_nat %in% unique_segs]]),
                      distance = as.numeric(dist[reservoir_subseg,network$subseg_id[network$seg_id_nat %in% unique_segs]])) %>%
    left_join(select(network, subseg_id, seg_id_nat)) %>%
    select(-geometry) %>%
    mutate(distance = distance/1000) %>%
    mutate(seg_id_nat = as.character(seg_id_nat))

  res_preds <- compare %>%
    left_join(distances)

  unique_sites <- unique(unlist(str_split(unique(res_preds$site_id), pattern = ', ')))
  unique_sites <- unique_sites[!is.na(unique_sites)]

  # get distances to specific sites (not segments)
  site_distances <- filter(cross, site_id %in% unique_sites) %>%
    select(seg_id_nat, site_id, fish_dist_to_outlet_m) %>%
    mutate(fish_dist_to_outlet_km = fish_dist_to_outlet_m/1000) %>%
    st_drop_geometry() %>%
    distinct()

  # calculate adjustment
  obs_adjustment <-  select(res_preds, seg_id_nat, date, sntemp_temp_c, mean_temp_c, subseg_id, distance, site_id) %>%
    left_join(select(res, date, res_bottom_temp = temp)) %>%
    # need both reservoir temp and stream temp to make comparison
    filter(!is.na(res_bottom_temp)) %>%
    filter(!is.na(mean_temp_c)) %>%
    rowwise() %>%
    # calculate distance from res to observation sites. Take mean of all listed sites when multiple listed
    mutate(site_distance = distance - mean(site_distances$fish_dist_to_outlet_km[site_distances$site_id %in% unlist(str_split(unique(site_id), pattern = ', '))])) %>%
    # this equation comes from solving
    # the equation for calculating stream temp which
    # we assume to be a combination of "reservoir temperature processes" and "stream temperature processes"
    # observed stream temp = obs_adjustment*res_bottom_temp + (1-obs_adjustment)*Modeled_Stream_temp
    mutate(obs_adjustment = (mean_temp_c - sntemp_temp_c)/(res_bottom_temp - sntemp_temp_c)) %>%
    # sometimes get infinite because obs_adjustment is negative, and you cant take the root of a negative number
    # if observed adjustment is <0, set it to 0
    # if observed adjustement is >1, set to 1
    # this constrains the predicted value to somewhere between no reservoir (weight = 0) and all reservoir info (weight = 1)
    mutate(obs_adjustment_constrained = case_when(obs_adjustment < 0 ~ 0,
                                                  obs_adjustment > 1 ~ 1,
                                                  TRUE ~ obs_adjustment)) %>%
    # now solve for the observed exponential decay rate (b) using our observed adjustment values
    # this equation comes from solving the exponential decay function
    # y=a(1-b)^x where x is the distance, b is the decay rate, and a is the starting value
    # a = 1 since we start with a weight of 1 for the reservoir temperature at the outlet of the reservoir,
    # and assume it exponentially decays with distance as water moves down the stream
    mutate(obs_rate_constrained = (-1*(obs_adjustment_constrained^(1/site_distance))) + 1,
           obs_rate = (-1*(obs_adjustment^(1/site_distance))) + 1) %>%
    mutate(doy = lubridate::yday(date), year = lubridate::year(date)) %>% ungroup()

  browser()

  head(obs_adjustment)

  df <- tibble(t = obs_adjustment$site_distance,
               y = obs_adjustment$obs_adjustment,
               yobs = obs_adjustment$obs_adjustment_constrained, sensor = 1) %>%
    arrange(-t)
  fitted <- df %>%
    nest(-sensor) %>%
    mutate(
      fit = map(data, ~nls(yobs ~ SSasymp(t, yf, y0, log_alpha), data = .)),
      tidied = map(fit, tidy),
      augmented = map(fit, augment),
    )
  library(drc)
  library(aomisc)
  model <- drm(yobs ~ t, fct = EXD.2(),
               data = df)

  fit <- nls(y ~ (1-r)^t,
             data = df,
             algorithm = 'port',
             start=c(r = 0.05), lower=c(r = 0), upper = c(r=1))
  df$pred <- predict(fit)
  plot(df$yobs~df$t)
  lines(df$t, df$pred, col = 'blue')

  fit2 <- nls(y ~ exp(-r*t),
             data = df,
             algorithm = 'port',
             start=c(r = 0.05), lower=c(r = 0), upper = c(r=1))


  summary(fit)

  plot(df$y ~ df$t, log = '')
  plot(fit, log = '', add = TRUE)
  fitted %>%
    unnest(tidied) %>%
    select(sensor, term, estimate) %>%
    spread(term, estimate) %>%
    mutate(alpha = exp(log_alpha))
  # now return the median adjusted and unadjusted decay rate
  # break out values by test/train splits
  decay_rates <- obs_adjustment %>%
    summarize(median_obs_rate_contrained = median(obs_rate_constrained[!date %in% test_period], na.rm = TRUE),
              median_obs_rate = median(obs_rate[!date %in% test_period], na.rm = TRUE))

  # use the same rate adjustments for Pepacton
  # calculate adjustment
  glm_preds <- readr::read_csv(glm_preds_file) %>%
    rename(date = time, glm_temp_c = temp)

  dwallin_full <-  select(res_preds, seg_id_nat, date, sntemp_temp_c, mean_temp_c, subseg_id, distance, site_id) %>%
    left_join(glm_preds) %>%
    # need both reservoir temp and stream temp to make comparison
    filter(!is.na(glm_temp_c)) %>%
    mutate(seg_adjustment = (1-decay_rates$median_obs_rate)^distance) %>%
    mutate(seg_adjustment_con = (1-decay_rates$median_obs_rate_contrained)^distance) %>%
    mutate(dwallin_temp_c = (glm_temp_c*seg_adjustment) + (sntemp_temp_c*(1-seg_adjustment)),
           dwallin_con_temp = (glm_temp_c*seg_adjustment_con) + (sntemp_temp_c*(1-seg_adjustment_con))) %>%
    mutate(year = lubridate::year(date),
           doy = lubridate::yday(date))

  dat_out <- select(dwallin_full, seg_id_nat, subseg_id, date, sntemp_temp_c, glm_temp_c, dwallin_temp_c, mean_temp_c, distance, doy, year)

  return(list(dat_out, decay_rates))
}

# applying these functions to seperate res/data

# segments below each reservoir
segs_cannon <- c('1566', '1563', '1565' , '1571', '1572')
segs_pep <- c('1444', '1450', '1459', '1461', '1462', '1463')

# nhdid of each reservoir
cannon_nhdid = 'nhdhr_120022743'
pep_nhdid = 'nhdhr_151957878'

# subsegment ID of reservoir reach
can_subseg <- '128_1'
pep_subseg <- '15_1'

library(tidyverse)
library(scipiper)
library(sf)
test_period <- c(seq(as.Date('1979-10-01'), as.Date('1984-09-30'), 1),
                 seq(as.Date('2010-10-01'), as.Date('2015-09-30'), 1),
                 seq(as.Date('2020-10-01'), as.Date('2021-09-30'), 1))

cannon_pred_outflow <- calc_decay(segs_downstream = segs_cannon,
                                  reservoir_nhdid = cannon_nhdid,
                                  test_period = test_period,
                                  glm_preds_file = 'miscellaneous/downstream_preds_nhdhr_120022743.csv',
                                  reservoir_subseg = can_subseg)
cannon_obs_outflow <- calc_decay(segs_downstream = segs_cannon,
                                  reservoir_nhdid = cannon_nhdid,
                                  test_period = test_period,
                                  glm_preds_file = 'miscellaneous/downstream_preds_nhdhr_120022743_obs_spill_vol.csv',
                                  reservoir_subseg = can_subseg)

head(cannon_obs_outflow[[1]])

# plots
plot_dat <- cannon_obs_outflow[[1]]
ggplot(filter(plot_dat, year %in% 2014, seg_id_nat %in% '1566'), aes(x = doy, y = sntemp_temp_c)) +
  geom_line(color = 'goldenrod') +
  geom_point(data = filter(plot_dat, year %in% 2014, seg_id_nat %in% '1566'), aes(x = doy, y = mean_temp_c)) +
  geom_line(data = filter(plot_dat, year %in% 2014, seg_id_nat %in% '1566'), aes(x = doy, y = dwallin_temp_c), color = 'darkolivegreen4') +
  geom_line(data = filter(plot_dat, year %in% 2014), aes(x = doy, y = glm_temp_c), color = 'royalblue3') +
  theme_bw() +
  labs(#subtitle = 'Temperature observations from streams below Cannonsville (filled dots). \nReservior outflow temperature predictions from GLM (black line). \nPRMS-SNTemp predictions from each segment are solid lines of the same color. \nDWALLIN predictions from each segment are dotted lines of the same color.',
       x = 'Day of year in 2014', y = 'Temperature [deg C]')

cannon_pred_outflow[[1]] %>%
  filter(date %in% test_period) %>%
  mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
         sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
  group_by(seg_id_nat) %>%
  summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
            rmse_dwallin = sqrt(mean(sqerror_dwallin)),
            distance_downstream = unique(distance),
            n = n()) %>%
  arrange(distance_downstream) %>%
  filter(n > 10)

cannon_obs_outflow[[1]] %>%
  filter(date %in% test_period) %>%
  mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
         sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
  group_by(seg_id_nat) %>%
  summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
            rmse_dwallin = sqrt(mean(sqerror_dwallin)),
            distance_downstream = unique(distance),
            n = n()) %>%
  arrange(distance_downstream) %>%
  filter(n > 10)

pep_pred_outflow <- calc_decay(segs_downstream = segs_pep,
                                  reservoir_nhdid = pep_nhdid,
                                  test_period = test_period,
                                  glm_preds_file = 'miscellaneous/downstream_preds_nhdhr_151957878.csv',
                                  reservoir_subseg = pep_subseg)
pep_obs_outflow <- calc_decay(segs_downstream = segs_pep,
                                 reservoir_nhdid = pep_nhdid,
                                 test_period = test_period,
                                 glm_preds_file = 'miscellaneous/downstream_preds_nhdhr_151957878_obs_spill_vol.csv',
                                 reservoir_subseg = pep_subseg)

pep_pred_outflow[[1]] %>%
  filter(date %in% test_period) %>%
  mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
         sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
  group_by(seg_id_nat) %>%
  summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
            rmse_dwallin = sqrt(mean(sqerror_dwallin)),
            distance_downstream = unique(distance),
            n = n()) %>%
  arrange(distance_downstream) %>%
  filter(n > 10)

pep_obs_outflow[[1]] %>%
  filter(date %in% test_period) %>%
  mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
         sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
  group_by(seg_id_nat) %>%
  summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
            rmse_dwallin = sqrt(mean(sqerror_dwallin)),
            distance_downstream = unique(distance),
            n = n()) %>%
  arrange(distance_downstream) %>%
  filter(n > 10)

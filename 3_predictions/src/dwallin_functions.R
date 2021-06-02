calc_decay <- function(segs_downstream,
                       reservoir_nhdid,
                       preds_obs_ind = '3_predictions/out/compare_predictions_obs.feather.ind',
                       glm_preds_file,
                       test_period,
                       distance_file = '1_network/out/subseg_distance_matrix.rds.ind',
                       crosswalk_ind = '2_observations/out/crosswalk_site_reach.rds.ind',
                       network_file = '1_network/out/network.rds.ind',
                       reservoir_subseg) {

  segs_keep <- segs_downstream

  # get PRMS-SNTemp predictions and paired observations
  compare <- feather::read_feather(sc_retrieve(preds_obs_ind)) %>%
    mutate(date = as.Date(date)) %>%
    filter(seg_id_nat %in% segs_keep) %>%
    mutate(doy = lubridate::yday(date),
           year = lubridate::year(date))

  # stream network
  network <- readRDS(sc_retrieve(network_file, 'getters.yml'))[[1]]

  # crosswalk between segs and sites
  cross <- readRDS(sc_retrieve(crosswalk_ind))

  # stream dat
  stream_dat <- compare
  unique_segs <- unique(stream_dat$seg_id_nat)

  # distance matrix to find distance downstream of reservoirs to be
  # used in exponential decay function
  dist <- readRDS(sc_retrieve(distance_file, 'getters.yml'))[[2]]

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
  # these will be used for training, but not for prediction
  site_distances <- filter(cross, site_id %in% unique_sites) %>%
    select(seg_id_nat, site_id, fish_dist_to_outlet_m) %>%
    mutate(fish_dist_to_outlet_km = fish_dist_to_outlet_m/1000) %>%
    st_drop_geometry() %>%
    distinct()

  # get GLM reservoir predictions
  glm_preds <- readr::read_csv(sc_retrieve(glm_preds_file, 'getters.yml')) %>%
    rename(date = time, glm_temp_c = temp) %>%
    filter(res_id %in% reservoir_nhdid)

  # calculate adjustment
  obs_adjustment <-  select(res_preds, seg_id_nat, date, sntemp_temp_c, mean_temp_c, subseg_id, distance, site_id) %>%
    left_join(glm_preds, by = 'date') %>%
    # need both reservoir temp and stream temp to make comparison
    filter(!is.na(glm_temp_c)) %>%
    filter(!is.na(mean_temp_c)) %>%
    rowwise() %>%
    # calculate distance from res to observation sites. Take mean of all listed sites when multiple listed
    mutate(site_distance = distance - mean(site_distances$fish_dist_to_outlet_km[site_distances$site_id %in% unlist(str_split(unique(site_id), pattern = ', '))])) %>%
    # this equation comes from solving
    # the equation for calculating stream temp which
    # we assume to be a combination of "reservoir temperature processes" and "stream temperature processes"
    # observed stream temp = res_weight*glm_temp_c + (1-res_weight)*Modeled_Stream_temp
    mutate(obs_weight = (mean_temp_c - sntemp_temp_c)/(glm_temp_c - sntemp_temp_c)) %>%
    # sometimes get infinite because obs_adjustment is negative, and you cant take the root of a negative number
    # if observed weight is <0, set it to 0
    # if observed weight is >1, set to 1
    # this constrains the predicted value to somewhere between no reservoir (weight = 0) and all reservoir info (weight = 1)
    mutate(obs_weight_constrained = case_when(obs_weight < 0 ~ 0,
                                                  obs_weight > 1 ~ 1,
                                                  TRUE ~ obs_weight))


  # fit the decay rate only on training period
  train_obs_adjustment <- filter(obs_adjustment, !date %in% test_period)

  # fit the exponential decay model
  # the model is simplified because we know start and end weights (1, 0)
  # so know the asmyptote and intercept
  # only solving for decay rate
  fit <- nls(obs_weight ~ exp(-r*site_distance),
             data = filter(train_obs_adjustment, obs_weight > 0 & obs_weight < 1),
             algorithm = 'port',
             start=c(r = 0.05), lower=c(r = 0), upper = c(r=1))
  fit_constrained <- nls(obs_weight_constrained ~ exp(-r*site_distance),
                                data = train_obs_adjustment,
                                algorithm = 'port',
                                start=c(r = 0.05), lower=c(r = 0), upper = c(r=1))


  # return the observed and observed constrained decay rates
  decay_rates <- c('obs_rate' = as.numeric(fit$m$getPars()), 'obs_rate_cons' = as.numeric(fit_constrained$m$getPars()))

  # make predictions and return values
  dwallin_full <-  select(res_preds, seg_id_nat, date, sntemp_temp_c, mean_temp_c, subseg_id, site_distance = distance, site_id) %>%
    left_join(glm_preds, by = 'date') %>%
    filter(!is.na(glm_temp_c))

  dwallin_full$res_weight <- predict(fit, dwallin_full)
  dwallin_full$res_weight_cons <- predict(fit_constrained, dwallin_full)


  print("DWALLIN calculations made with 'constrained' estimates, where observed weights were constrained to values between 0 and 1.")
  dwallin_full <- dwallin_full %>%
    mutate(dwallin_temp_c = (glm_temp_c*res_weight_cons) + (sntemp_temp_c*(1-res_weight_cons)),
           year = lubridate::year(date),
           doy = lubridate::yday(date))


  performance <- ungroup(dwallin_full) %>%
    filter(date %in% test_period) %>%
    filter(!is.na(mean_temp_c)) %>%
    mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
           sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
    group_by(seg_id_nat) %>%
    summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
              rmse_dwallin = sqrt(mean(sqerror_dwallin)),
              distance_downstream = unique(site_distance),
              n = n()) %>%
    filter(n > 10) %>% arrange(distance_downstream)

  print("Performance of DWALLIN model versus PRMS-SNTemp at downstream segments:")
  print(performance)

  dat_out <- select(dwallin_full, seg_id_nat, subseg_id, date, sntemp_temp_c, glm_temp_c, dwallin_temp_c, mean_temp_c, distance = site_distance, doy, year)

  return(list(dat_out, decay_rates))
}



# plots
# plot_dat <- cannon_obs_outflow[[1]]
# ggplot(filter(plot_dat, year %in% 2014, seg_id_nat %in% '1566'), aes(x = doy, y = sntemp_temp_c)) +
#   geom_line(color = 'goldenrod') +
#   geom_point(data = filter(plot_dat, year %in% 2014, seg_id_nat %in% '1566'), aes(x = doy, y = mean_temp_c)) +
#   geom_line(data = filter(plot_dat, year %in% 2014, seg_id_nat %in% '1566'), aes(x = doy, y = dwallin_temp_c), color = 'darkolivegreen4') +
#   geom_line(data = filter(plot_dat, year %in% 2014), aes(x = doy, y = glm_temp_c), color = 'royalblue3') +
#   theme_bw() +
#   labs(#subtitle = 'Temperature observations from streams below Cannonsville (filled dots). \nReservior outflow temperature predictions from GLM (black line). \nPRMS-SNTemp predictions from each segment are solid lines of the same color. \nDWALLIN predictions from each segment are dotted lines of the same color.',
#        x = 'Day of year in 2014', y = 'Temperature [deg C]')
#
# cannon_pred_outflow[[1]] %>%
#   filter(date %in% test_period) %>%
#   mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
#          sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
#   group_by(seg_id_nat) %>%
#   summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
#             rmse_dwallin = sqrt(mean(sqerror_dwallin)),
#             distance_downstream = unique(distance),
#             n = n()) %>%
#   arrange(distance_downstream) %>%
#   filter(n > 10)
#
# cannon_obs_outflow[[1]] %>%
#   filter(date %in% test_period) %>%
#   mutate(sqerror_dwallin = (dwallin_temp_c - mean_temp_c)^2,
#          sqerror_sntemp = (sntemp_temp_c - mean_temp_c)^2) %>%
#   group_by(seg_id_nat) %>%
#   summarize(rmse_sntemp = sqrt(mean(sqerror_sntemp)),
#             rmse_dwallin = sqrt(mean(sqerror_dwallin)),
#             distance_downstream = unique(distance),
#             n = n()) %>%
#   arrange(distance_downstream) %>%
#   filter(n > 10)
#


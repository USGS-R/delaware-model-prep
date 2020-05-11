# NGWOS experiments

# read in data from Jeff
# for now, downloaded these files from here: https://drive.google.com/drive/u/0/folders/1Fu4m01la28Vp8zI33Dg-1tzGJbFotf8c
noNGWOS_train_file <- '3_predictions/in/rgcn_v2_preds_no_ngwos_trn.feather'
NGWOS_train_file <- '3_predictions/in/rgcn_v2_preds_w_ngwos_trn.feather'

noNGWOS_test_file <- '3_predictions/in/rgcn_v2_preds_no_ngwos.feather'
NGWOS_test_file <- '3_predictions/in/rgcn_v2_preds_w_ngwos.feather'

# get ngwos sites and reaches
ngwos_reaches <- scmake('ngwos_reaches')
ngwos_sites <- scmake('ngwos_sites')

# add period and run indicators so can bind data together
# train was 1993-10-01 to 2019-09-24  -- why not 12-31

noNGWOS_train <- feather::read_feather(noNGWOS_train_file) %>%
  mutate(period = 'train', run = 'noNGWOS')

NGWOS_train <- feather::read_feather(NGWOS_train_file) %>%
  mutate(period = 'train', run = 'NGWOS')

noNGWOS_test <- feather::read_feather(noNGWOS_test_file) %>%
  mutate(period = 'test', run = 'noNGWOS')

NGWOS_test <- feather::read_feather(NGWOS_test_file) %>%
  mutate(period = 'test', run = 'NGWOS')

drb_dat <- readRDS('2_observations/out/obs_temp_drb.rds') %>%
  mutate(seg_id_nat = as.character(seg_id_nat))

# how much data is NGWOS data in training period?
train_dat <- filter(drb_dat, date > as.Date('1993-09-30'))
train_no_ngwos <- filter(train_dat, !(seg_id_nat %in% ngwos_reaches & date > as.Date('2017-10-01')))

(nrow(train_dat) - nrow(train_no_ngwos))/nrow(train_dat)

# how many sites are NGWOS sites in "NGWOS" period?
# all drb_dat is before reducing sites to those that match closely to network
all_drb <- readRDS('2_observations/out/all_drb_temp_obs.rds') %>%
  filter(!source %in% 'wqp') %>%
  mutate(in_ngwos= site_id %in% ngwos_sites) %>%
  filter(date >= as.Date('2017-10-01')) %>%
  group_by(in_ngwos) %>%
  summarize(n_sites = length(unique(site_id)))


# merge predictions and observations
compare <- bind_rows(noNGWOS_test, NGWOS_test, noNGWOS_train, NGWOS_train) %>%
  mutate(seg_id_nat = as.character(seg_id_nat),
         date = as.Date(date)) %>%
  rename(pred_temp_c = temp_degC) %>%
  left_join(drb_dat)

# calculate statistics
error <- compare %>%
  filter(!is.na(temp_c)) %>%
  mutate(error = temp_c - pred_temp_c,
         sq_error = error^2)

# summarize stats by run and period
stats <- error %>%
  group_by(run, period) %>%
  summarize(RMSE = sqrt(mean(sq_error)),
            NSE = 1 - (sum(sq_error)/sum((temp_c - mean(temp_c))^2)),
            bias = mean(error))

# break down run and period stats by reach
stats_by_reach<- error %>%
  group_by(run, period, seg_id_nat) %>%
  summarize(RMSE = sqrt(mean(sq_error)),
            NSE = 1 - (sum(sq_error)/sum((temp_c - mean(temp_c))^2)),
            bias = mean(error),
            n = n()) %>%
  ungroup() %>%
  select(-NSE, -bias) %>%
  tidyr::spread(key = run, value = RMSE) %>%
  mutate(RMSE_diff = NGWOS- noNGWOS) %>%
  mutate(ngwos_reach = seg_id_nat %in% ngwos_reaches)

# view reach results of 3 withheld sites
View(filter(stats_by_reach, seg_id_nat %in% c('1573', '1718', '1568')))

network <- readRDS('1_network/out/network.rds')
network <- network$edges

rmse_network <- network %>%
  mutate(seg_id_nat = as.character(seg_id_nat)) %>%
  left_join(filter(stats_by_reach, period %in% 'test'))

# plot RMSE difference 
ggplot(rmse_network) +
  geom_sf(aes(color = RMSE_diff), size = 1) +
  geom_sf(data = filter(rmse_network, seg_id_nat %in% ngwos_reaches), aes(color = RMSE_diff), size = 1.9) +
  scale_color_gradient2(low = 'blue', mid = 'white', high = 'red', na.value = 'gray80') +
  theme_bw() +
  map_theme +
  labs(color = 'RMSE difference')

reach_stats_lots_obs <- filter(stats_by_reach, n>=20 & period %in% 'test')
rmse_network2 <- network %>%
  mutate(seg_id_nat = as.character(seg_id_nat)) %>%
  left_join(reach_stats_lots_obs)

ggplot(rmse_network2) +
  geom_sf(aes(color = NGWOS), size = 1) +
  geom_sf(data = filter(rmse_network, seg_id_nat %in% ngwos_reaches), aes(color = NGWOS), size = 1.9) +
  scale_color_viridis_c(direction = -1, option = 'inferno', na.value = 'lightgray') +
  theme_bw() +
  map_theme +
  labs(color = 'RMSE')

# make time series plots, function below

viz_time_performance(compare = compare, reach = '1784', fileout = '1784_NGWOS_experiment_timeseries.png')
viz_time_performance(compare = compare, reach = '1784', fileout = '1784_NGWOS_experiment_timeseries.png')
viz_time_performance(compare = compare, reach = '1784', fileout = '1784_NGWOS_experiment_timeseries.png')


viz_time_performance <- function(compare, start = "2018-01-20", end = "2019-07-27", reach, fileout){
  png(fileout, width = 12.9, height = 7.5, units = 'in', res = 250) # not quite to the edge
  
  #layout(matrix(c(1,1,1,2),byrow = TRUE))
  
  par(omi = c(0.1,0,0.3,0), mai = c(0.6,1,0,0), las = 1, mgp = c(3.3,0.8,0))
  
  plot(NA, NA, xlim = as.Date(c(start, end)), ylim = c(-0.5,30), xlab = "",
       ylab = 'Water temperature (°C)', axes = FALSE, xaxs = 'i', yaxs = 'i', cex.lab = 2)
  
  axis(2, at = seq(-50,30, by = 5), las = 1, tck = -0.01, cex.axis = 1.5, lwd = 1.5)
  
  date_ticks <- seq(lubridate::floor_date(as.Date(start), unit = 'month'), to = lubridate::ceiling_date(as.Date(end), unit = 'month'), 'month')
  
  
  axis(1, date_ticks, labels = NA, tck = -0.01, lwd = 1.5)
  par(mgp = c(3.3,0.2,0))
  axis(1, date_ticks+15, labels = format(date_ticks, '%b'), tck = NA, lwd = NA, cex.axis = 1.1)
  
  date_ticks <- seq(lubridate::floor_date(as.Date(start), unit = 'year'), to = lubridate::ceiling_date(as.Date(end), unit = 'year'), 'year')
  
  axis(1, date_ticks, labels = NA, tck = -0.05, lwd = 1.5)
  par(mgp = c(3.3,2,0))
  axis(1, date_ticks+365/2, labels = format(date_ticks, '%Y'), cex.axis = 2, tck = NA, lwd = NA)
  
  x_mod <- compare %>% filter(seg_id_nat == reach & run %in% 'noNGWOS') %>% pull(date)
  y_noNGWOS <- compare %>% filter(seg_id_nat == reach & run %in% 'noNGWOS') %>% pull(pred_temp_c)
  y_NGWOS <- compare %>% filter(seg_id_nat == reach & run %in% 'NGWOS') %>% pull(pred_temp_c)
  
  x_obs <- compare %>% filter(seg_id_nat == reach & run %in% 'noNGWOS') %>% pull(date)
  y_obs <- compare %>% filter(seg_id_nat == reach & run %in% 'noNGWOS') %>% pull(temp_c)
  
  
  
  points(x_mod, y_noNGWOS, pch = 20, col = '#8983d4', type = "l", lwd = 3, cex = 0.3) 
  
  points(x_mod, y_NGWOS, pch = 20, col = '#47446e', type = "l", lwd = 3, cex = 0.3)
  
  points(x_obs, y_obs, pch = 20, col = 'black')
  dev.off()
}

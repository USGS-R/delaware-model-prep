library(RcppCNPy)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scipiper)

prep_model_compare <- function() {
  # put XJ's predictions in folder 3_predictions/in and read in
  dat_all <- npyLoad('3_predictions/in/prd_RGCN_full_obstemp_cv2_full.npy')
  dat_02 <- npyLoad('3_predictions/in/prd_RGCN_full_obstemp_cv2_002.npy')
  
  # transform and munge npy data
  munge_npy <- function(dat) {
    tdat <- as.data.frame(t(dat))
    names(tdat) <- c('2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012',
                     '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020',
                     '2021', '2022', '2023', '2024', '2025', '2026', '2027', '2028',
                     '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037',
                     '2038', '2039', '2040', '2041', '2044', '2045', '2046', '2047',
                     '2048', '4182')
    tdat$date <- seq.Date(from = as.Date('2004-10-01'), to = as.Date('2016-09-30'), by = 1)
    
    dat_long <- tidyr::gather(tdat, key = 'seg_id_nat', value = 'pgrnn_temp_c', -date)
    return(dat_long)
  }
  
  pgrnn_all <- munge_npy(dat_all) %>% rename(pgrnn_all = pgrnn_temp_c)
  pgrnn_02 <- munge_npy(dat_02) %>% rename(pgrnn_02 = pgrnn_temp_c)
  
  pgrnn <- left_join(pgrnn_all, pgrnn_02)
  
  # join with SNTemp predictions
  sntemp <- read.csv('3_predictions/out/uncal_sntemp_preds.csv', stringsAsFactors = FALSE) %>%
    tidyr::gather(key = 'seg_id_nat', value = 'pred_temp_degC', -Date) %>%
    mutate(seg_id_nat = gsub('X', '', seg_id_nat),
           date = as.Date(Date)) %>% select(-Date)
  
  compare <- left_join(rename(sntemp, sntemp = pred_temp_degC), pgrnn)
  return(compare)
}



# merge in observations in this geographic subset
obs <- read.csv('9_collaborator_data/umn/obs_temp_subset.csv', 
                stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date), seg_id_nat = as.character(seg_id_nat))

# warm events
above_29 <- filter(obs, temp_c >=29) %>%
  filter(date >= as.Date('2004-10-01'))

# grab an event and plot
temp_obs <- filter(obs, seg_id_nat %in% above_29$seg_id_nat[1]) %>%
  filter(date >= (above_29$date[1] - 10) & date <= (above_29$date[1] + 10))

temp_compare <- filter(compare, seg_id_nat %in% above_29$seg_id_nat[1]) %>%
  filter(date >= (above_29$date[1] - 10) & date <= (above_29$date[1] + 10))

# let's view all of the times that 
# plot function
compare_output <- function(pred_dat, obs_dat, reach = '2007', start = "2006-01-01", end = "2008-12-31") {
  
  temp_preds <- filter(pred_dat, date <= as.Date(end) & date >= (start)) %>%
    filter(seg_id_nat %in% reach) %>%
    tidyr::gather(key = 'model', value = 'temp_c', -date, -seg_id_nat)
  
  temp_obs <- filter(obs_dat, date <= as.Date(end) & date >= (start)) %>%
    filter(seg_id_nat %in% reach)
  
  '#e08214'
  p <- ggplot(temp_preds, aes(x = date, y = temp_c)) +
    geom_line(aes(color = model, group = model)) +
    geom_point(data = temp_obs, aes(x = date, y = temp_c), size = 0.8) +
    scale_color_manual(values = c('sntemp' = '#e08214', 'pgrnn_all' = '#542788', 'pgrnn_02' = '#b2abd2')) +
    theme_bw() +
    labs(x = '', y = "Pred. or Obs. Temperature [deg C]")
  
  print(p)
}

# example plot
for (i in 1:nrow(above_29)) {
   compare_output(compare, obs, reach = above_29$seg_id_nat[i], start = above_29$date[i]-40, end = above_29$date[i]+40)
  
  
}

obs_abv_75 <- filter(obs, temp_c >=23.89) %>%
  filter(date >= as.Date('2004-10-01')) %>%
  left_join(compare)



obs_year <- mutate(obs, year = lubridate::year(date)) %>%
  group_by(seg_id_nat, year) %>%
  summarize(n_per_year = n()) %>%
  filter(n_per_year > 300)

max_temp_timing <- mutate(obs, year = lubridate::year(date)) %>%
  group_by(seg_id_nat, year) %>%
  summarize(n_per_year = n(),
            max_temp = max(temp_c),
            max_timing = lubridate::yday(date[which.max(temp_c)]),
            max_date = date[which.max(temp_c)],
            summer_complete = all(170:245 %in% lubridate::yday(date))) %>%
  filter(year > 2004 & year < 2016 & summer_complete)


sntemp_max <- mutate(sntemp, year = lubridate::year(date)) %>%
  group_by(seg_id_nat, year) %>%
  summarize(max_temp_sntemp = max(pred_temp_degC),
            max_timing_sntemp = lubridate::yday(date[which.max(pred_temp_degC)]))

pgrn_max <- mutate(pgrnn_all, year = lubridate::year(date)) %>%
  group_by(seg_id_nat, year) %>%
  summarize(max_temp_pgrn = max(pgrnn_all),
            max_timing_pgrn = lubridate::yday(date[which.max(pgrnn_all)]))

compare_max <- left_join(max_temp_timing, sntemp_max) %>%
  left_join(pgrn_max)

compare_rel <- compare_max %>%
  mutate(max_timing_sntemp = max_timing_sntemp - max_timing, 
         max_temp_sntemp = max_temp_sntemp - max_temp,
         max_timing_pgrn = max_timing_pgrn - max_timing,
         max_temp_pgrn = max_temp_pgrn - max_temp,
         timing = 0, 
         temp = 0)

compare_rel_timing <- select(compare_rel, seg_id_nat, year, timing, max_timing_sntemp, max_timing_pgrn) %>%
  tidyr::gather(key = 'model', value = 'max_timing_modeled', -seg_id_nat, -year, -timing) %>%
  mutate(model = ifelse(grepl('sntemp', model), 'SNTEMP (process model)', 'PGDL (hybrid model)'))

compare_rel_temp <- select(compare_rel, seg_id_nat, year, temp, max_temp_sntemp, max_temp_pgrn) %>%
  tidyr::gather(key = 'model', value = 'max_temp_modeled', -seg_id_nat, -year, -temp) %>%
  mutate(model = ifelse(grepl('sntemp', model), 'SNTEMP (process model)', 'PGDL (hybrid model)'))

compare_rel <- left_join(compare_rel_timing, compare_rel_temp) %>%
  ungroup()

compare_rel$model <- factor(compare_rel$model, levels = c('PGDL (hybrid model)', 'SNTEMP (process model)'))

p <- ggplot(compare_rel, aes(x = max_timing_modeled, y = max_temp_modeled)) +
  geom_point() +
  geom_hline(yintercept = 0, color = 'red') +
  geom_vline(xintercept = 0, color = 'red') +
  scale_color_manual(values = c('#d95f02', '#1b9e77')) +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        title = element_text(size = 20)) +
  gganimate::transition_states(states = compare_rel$model) +
  labs(x = 'Timing of max temp (error in days)', y = 'Max temp (error in degrees)',
       title = '{closest_state}')

gganimate::animate(p, width = 1500, height = 1500, res = 300)
anim_save('max_timing_temp.gif')

gganimate::animate(p, width = 1500, height = 1500, res = 300, nframes = 1)
anim_save('max_timing_temp_empty.gif')

gganimate::animate(p, width = 1500, height = 1500, res = 300, nframes = 1)
anim_save('max_timing_temp_empty2.gif')

p2 <- ggplot(compare_rel, aes(x = max_timing_modeled, y = max_temp_modeled)) +
  geom_point(alpha = 0) +
  geom_point(data = filter(compare_rel, model %in% 'SNTEMP (process model)')) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_vline(xintercept = 0, color = 'red') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        title = element_text(size = 20)) +
  #gganimate::transition_states(states = compare_rel$model) +
  labs(x = 'Timing of max temp (error in days)', y = 'Max temp (error in degrees)',
       title = 'SNTEMP (process model)')

ggsave('timing_temp_blank.png', p2, height = 5, width = 5, dpi = 300)
p3 <- ggplot(compare_rel, aes(x = max_timing_modeled, y = max_temp_modeled)) +
  geom_point(alpha = 0) +
  geom_point(data = filter(compare_rel, model %in% 'PGDL (hybrid model)')) +
  geom_hline(yintercept = 0, color = 'red') +
  geom_vline(xintercept = 0, color = 'red') +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        title = element_text(size = 20)) +
  #gganimate::transition_states(states = compare_rel$model) +
  labs(x = 'Timing of max temp (error in days)', y = 'Max temp (error in degrees)',
       title = 'PGDL (hybrid model)')

ggsave('timing_temp_blank2.png', p3, height = 5, width = 5, dpi = 300)


compare_timing <- select(compare_max, seg_id_nat, year, max_timing, max_timing_sntemp, max_timing_pgrn) %>%
  tidyr::gather(key = 'model', value = 'max_timing_modeled', -seg_id_nat, -year, -max_timing) %>%
  mutate(error = max_timing - max_timing_modeled,
         error_within_week = abs(error)<=7)

compare_maxtemp <-  select(compare_max, seg_id_nat, year, max_temp, max_temp_sntemp, max_temp_pgrn) %>%
  tidyr::gather(key = 'model', value = 'max_temp_modeled', -seg_id_nat, -year, -max_temp) %>%
  mutate(error = max_temp - max_temp_modeled,
         error_within_1 = abs(error)<=1)

ggplot(compare_timing, aes(y = max_timing_modeled, x = max_timing)) +
  geom_point(aes(color = error_within_week)) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  scale_color_manual(values = c('#d95f02', '#1b9e77')) +
  facet_wrap(~model, ncol = 1) +
  theme_bw() +
  labs(x = 'Observed maxtemp DOY', y = 'Modeled maxtemp DOY',
       color = 'Error < 1 week')

ggplot(compare_maxtemp, aes(y = max_temp_modeled, x = max_temp)) +
  geom_point(aes(color = error_within_1)) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  facet_wrap(~model, ncol = 1) +
  theme_bw() +
  labs(x = 'Observed maxtemp', y = 'Modeled maxtemp',
       color = 'Error < 1 deg')

rmse_maxtemp <- compare_maxtemp %>%
  mutate(sq_error = (max_temp - max_temp_modeled)^2) %>%
  group_by(model) %>%
  summarize(rmse = sqrt(mean(sq_error)))

rmse_timing <- compare_timing %>%
  mutate(sq_error = (max_timing - max_timing_modeled)^2) %>%
  group_by(model) %>%
  summarize(rmse = sqrt(mean(sq_error)))
head(rmse_maxtemp)

compare_output(compare, obs, reach = '2007', start = '2008-06-15', end = '2008-09-15')



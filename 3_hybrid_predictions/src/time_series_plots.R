## plotting time series.
#looping through seg_id_nats and plot date on x, temperature on y, and have observed data as dots and predicted data as lines

# filter the data by year 2000 then getting the unique segs idea and creating a for loop to plot.

library(tidyverse)
library(scipiper)
library(RcppCNPy)
library(lubridate)
library(dplyr)


dat <- readRDS(sc_retrieve('3_hybrid_predictions/out/compare_predictions_obs.rds.ind', 'getters.yml'))

# changing the order of the models by declaring a factor.
dat$model <- factor(dat$model, levels = c('ANN', 'RNN', 'RNN_ptrn', 'RGCN', 'RGCN_ptrn', 'RGCN_ptrn_ctr'))

# to select the year with the most observation.
# 1) remove NA   2) group by year  3) summarize to get number of observation.
yearly_nobs <-  dat %>%
  mutate(year = lubridate::year(date)) %>% #should always be right after we read the data.
  filter(!is.na(predicted_temp_c)) %>%
  filter(!is.na(temp_c)) %>%
  select(seg_id_nat, year, date, temp_c) %>%
  distinct() %>%
  group_by(year) %>%
  summarize(n_obs = n())

# to get the max number of observation and find the year associated with it.
year_max_obs <- yearly_nobs$year[which.max(yearly_nobs$n_obs)]
dat_2014 <- dat %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year %in% 2012:2014)

# find all the segs
segs <- unique(dat_2014$seg_id_nat)

# getting the number of obs for the data with selected year.
test_temp_dat <- filter(dat_2014, seg_id_nat %in% temp_seg) %>%
  group_by(seg_id_nat) %>%
  summarize(n_obs = n())

# to add the models name list 'ANN' = 'plain_neural_network'
# model names
model_names <- c("ANN" = 'ANN',
                 "RNN" = "+ time",
                 "RNN_ptrn" = " + time, prtrn",
                 "RGCN" = "+ time, space",
                 "RGCN_ptrn" = "+ time, space, prtrn",
                 "RGCN_ptrn_ctr" = "+ time, space, prtrn, ctr loss")

# to loop through the segs vector similar to using for (i in ...) without having to index in the rest of the loop.
for (temp_seg in segs) {
  temp_dat <- dat_2014 %>%
    filter(seg_id_nat %in% temp_seg)
#plotting each segs  obs & pred temps while grouping by model, x = dates (2014), y temp.
  p <- ggplot(data = temp_dat, aes(x = date, y = predicted_temp_c,
                              group = model, colour = model)) +
    geom_line(size = 1)+
    geom_point(data = temp_dat, aes(x = date, y = temp_c),
               alpha = 0.35, colour = "black", size = 1) +
    facet_wrap(~ model, strip.position = "top", ncol = 1,
               labeller = labeller(model = model_names)) +
    theme_bw() +
    cowplot::theme_cowplot() +
    labs(x = "Date",
         y = "Temperature") +
    ggtitle(paste0("Segment ", temp_seg)) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
          strip.background = element_blank())

    # to title each plot with a unique title that deals with segs-id and not write over plots
    #temp_filename <- paste0('timeseries_predicted_temp_seg_', temp_seg, '.png')
 #print(p)
  temp_out <- paste0("8_visualize/out/", 'timeseries_predicted_temp_seg_', temp_seg, '.png')
 ggsave(temp_out, p, height = 10, width = 10)
}



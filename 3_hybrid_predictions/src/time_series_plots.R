## plotting time series.
#looping through seg_id_nats and plot date on x, temperature on y, and have observed data as dots and predicted data as lines

# filter the data by year 2000 then getting the unique segs idea and creating a for loop to plot.

library(tidyverse)
library(scipiper)
library(RcppCNPy)
library(lubridate)
library(dplyr)


dat <- combine_preds_obs(obs_ind = '2_observations/out/obs_temp_drb.rds.ind',
                         #ann_npy = '3_hybrid_predictions/in/PGRNN_0716.npy',
                         rnn_npy = '3_hybrid_predictions/in/RNN_0518.npy',
                         rgnc_npy = '3_hybrid_predictions/in/RGCN_0518_woptr.npy',
                         rgnc_ptrn_npy = '3_hybrid_predictions/in/RGCN_0518_wctr.npy',
                         out_file = '3_hybrid_predictions/out/combine_test.csv')


# to select the year with the most observation.
# 1) remove NA   2) group by year  3) summarize to get number of observation.
subset_dat <-  dat %>%
  mutate(year = lubridate::year(date)) %>% #should always be right after we read the data.
  filter(!is.na(predicted_temp_c)) %>%
  filter(!is.na(temp_c)) %>%
  group_by(year) %>%
  summarize(n_obs = n())

# to get the max number of observation and find the year associated with it.
year_max_obs <- max(subset_dat)

segs <- unique(dat$seg_id_nat)

dat_2014 <- filter(dat, lubridate::year(date)) %in% 2014
segs <- unique(dat_2014$)
                   segs <- unique(dat_2014$seg_id_nat)
                   for (temp_seg in segs) {
                     temp_dat <- filter(dat_2000, seg_id_nat %in% temp_seg)
                     ggplot(temp_dat)...
                   }

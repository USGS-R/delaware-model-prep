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
dat$model <- factor(dat$model, levels = c('RNN', 'RGNC', 'RGCN_ptrn'))

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

dat_2014 <- filter(dat, lubridate::year(date) %in% 2014) %>%
  mutate(date = as.Date(date))
segs <- unique(dat_2014$seg_id_nat)

# getting the number of obs for the data with selected year.
test_temp_dat <- filter(dat_2014, seg_id_nat %in% temp_seg) %>%
  group_by(seg_id_nat) %>%
  summarize(n_obs = n())

# to add the models name list 'ANN' = 'plain_neural_network'
# model names
model_names <- c("RNN" = "+ time_awareness",
                    "RGNC" = "+ pretraining",
                    "RGCN_ptrn" = "+ space_awareness")

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
    ggtitle(paste0("Timeseries Temperature for Segment Id: ", temp_seg)) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none",
          strip.background = element_rect(fill = "gray96",
                                          color = "black"))

    # to title each plot with a unique title that deals with segs-id and not write over plots
    #temp_filename <- paste0('timeseries_predicted_temp_seg_', temp_seg, '.png')
 #print(p)
  temp_out <- paste0("8_visualize/out/", 'timeseries_predicted_temp_seg_', temp_seg, '.png')
 ggsave(temp_out, p, height = 7.5)
}

## PGDL figures and metrics sprint
# August 12, 2020
# Rasha Atshan first sprint with Alison, JOrdan, and Sam

##### refering to delware_model_prep issue 56 #####
# 1) Extracting data from the tables, then writing it to a csv file.
library(tidyr)
library(dplyr)
library(tidyverse)
library(shades)
#process guidance: summer and non-summer rmse table
non_summer_train <- data.frame(matrix(c('plain neural network', 'time awareness', 'pretraining', 'space awareness', 1.5, 1.3, 1.3, 1.3, 2.1, 2.1, 1.9, 1.7), nrow = 4, ncol = 3))
colnames(non_summer_train)[1] = 'model'
colnames(non_summer_train)[2] = 'Non_summer'
colnames(non_summer_train)[3] = 'Summer'
# writing the dataframe ti csv file
non_summer_train_dat <-readr::write_csv(non_summer_train, path = '8_visualize/in/non_summer_train_dat.csv')
# less need of data: rmse on test periods table.
subset_train <- data.frame(matrix(c('Uncalibrated process model', 'plain neural network', 'time awareness', 'pretraining', 'space awareness', 3.7, 1.6, 1.5, 1.44, 1.40, 3.7, 1.8, 1.7, 1.5, 1.43, 3.7, 2.2, 1.9, 1.8, 1.6), nrow = 5, ncol = 4))
colnames(subset_train)[1] = 'model'
colnames(subset_train)[2] = '100'
colnames(subset_train)[3] = '10'
colnames(subset_train)[4] = '2'
subset_train_dat <- readr::write_csv(subset_train, path = '8_visualize/in/subset_train_dat.csv')

#reading the data csv files and using pivot_long to change the data from wide to long.
non_summer_mod_dat <- readr::read_csv('8_visualize/in/non_summer_train_dat.csv') %>%
  pivot_longer(c(-model), names_to = 'test_period', values_to = 'rmse')

subset_mod_dat <- readr::read_csv('8_visualize/in/subset_train_dat.csv') %>%
  filter(!model %in%  'Uncalibrated process model') %>%
  pivot_longer(c(-model), names_to = 'dat_availability', values_to = 'rmse')
subset_mod_dat$dat_availability <- factor(subset_mod_dat$dat_availability, levels = c('2', '10', '100'))
subset_mod_dat$model <- factor(subset_mod_dat$model, levels = c('space awareness', 'pretraining', 'time awareness','plain neural network'))


#  line plot for `non-summer_mod_dat` with the 1, 10, 100% on the x, model metric on the y, and each model getting their own line.
color <- c('#47437e', '#7570b3', '#b3b0d5', '#d95f02')
plot_label <- c('+ space awareness', '+ pretraining', '+ time awareness','plain neural network')
  ggplot(data = subset_mod_dat, aes(x = dat_availability, y = rmse, group =     model, colour = model)) +
   geom_line() +
   geom_point(aes(shape = model))  +
   #scale_x_continuous(labels = c("2%", "10%", "100%")) +
   scale_y_continuous(trans = "reverse") +
   theme_bw()+
   theme(legend.title = element_blank())+
   scale_color_manual(values = color) +
    scale_shape_manual(labels= plot_label) +
    labs(x = 'Training Used Data',
         y = 'RMSE')










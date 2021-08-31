## PGDL figures and metrics sprint
# August 12, 2020
# Rasha Atshan first sprint with Alison, JOrdan, and Sam

library(tidyr)
library(dplyr)
library(tidyverse)
library(shades)

model_names <- c('space_awareness', 'pretraining', 'time_awareness', 'plain_neural_network')
plot_models <- setNames(c('+ space awareness', '+ pretraining', '+ time awareness', 'Plain neural network'), model_names)
plot_color <- setNames(c('#47437e', '#7570b3', '#b3b0d5', '#d95f02'), model_names)
plot_shape <- setNames(c(23, 24, 25, 22), model_names)

##### refering to delware_model_prep issue 56 #####

#### Out-of-bounds season figure ####

# 1) Extracting data from the tables, then writing it to a csv file.

#process guidance: summer and non-summer rmse table
season_train <- data.frame(matrix(c(
   'plain_neural_network', 'time_awareness', 'pretraining', 'space_awareness',
   2.138, 2.104, 1.893, 1.744,
   0.093, 0.080, 0.085, 0.053,
   1.794, 1.789, 1.555, 1.416,
   0.032, 0.034, 0.021, 0.019),
   nrow = 4, ncol = 5))
colnames(season_train)[1] = 'model'
colnames(season_train)[2] = 'Train_non_summer_rmse'
colnames(season_train)[3] = 'Train_non_summer_ci'
colnames(season_train)[4] = 'Train_all_seasons_rmse'
colnames(season_train)[5] = 'Train_all_seasons_ci'
# writing the dataframe to csv file
season_train_dat <-readr::write_csv(season_train, file = '8_visualize/in/season_train_dat.csv')

#reading the data csv files and using pivot_long to change the data from wide to long.
season_train_mod_dat <- readr::read_csv('8_visualize/in/season_train_dat.csv') %>%
   pivot_longer(c(-model), names_to = c('train_season', 'stat'), values_to = c('value'), names_pattern = "(.*)_([[:alpha:]]+$)")  %>%
   pivot_wider(names_from='stat', values_from='value')

season_train_mod_dat$model <- factor(
   season_train_mod_dat$model,
   levels = c('plain_neural_network', 'time_awareness', 'pretraining', 'space_awareness'))
season_train_mod_dat$train_season <- factor(
   season_train_mod_dat$train_season,
   levels = c('Train_all_seasons', 'Train_non_summer'))

#seasonal test as a bar plot.Probably color = model or color = season, with whatever is not used for color as the primary grouping variable for the bars (e.g., if color = season, bars should be grouped by model)

seasonal_test_plot <-
   ggplot(data = season_train_mod_dat, aes(x = train_season,
                                           y = rmse , fill = model, color=model)) +
   # using position = 'dodge' will place the bars next to each other.
   # geom_col(position = 'dodge')  +
   geom_boxplot() +
   geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci),
                 width = .2, position = position_dodge(.75)) +
   scale_fill_manual(labels = plot_models, values = plot_color) +
   scale_color_manual(labels = plot_models, values = plot_color) +
   scale_x_discrete(label = c("Train all seasons", "Train non-summer" )) +
   scale_y_reverse() +
   theme_bw() +
   theme_minimal() +
   cowplot::theme_cowplot(font_size = 14) +
   theme(legend.title = element_blank(), # to remove legend box title.
         axis.title.x = element_blank(),
         ## for legend box position
         #legend.position = c(.95, .95),
         #to place legend on top
         legend.justification="right",
         legend.box.just = "top") +
   labs( y = "Summer test RMSE (°C)")
seasonal_test_plot
ggsave("8_visualize/out/seasonal_plot.png", seasonal_test_plot, height = 5,
       width = 7, dpi = 250)

#### Sparsity figure ####

# less need of data: rmse on test periods table.
subset_train <- data.frame(matrix(c(
   'Uncalibrated_process_model', 'plain_neural_network', 'time_awareness', 'pretraining', 'space_awareness',
   3.661, 1.575, 1.546, 1.444, 1.402,
   0, 0.035, 0.045, 0.039, 0.034,
   3.661, 1.769, 1.706, 1.533, 1.434,
   0, 0.047, 0.049, 0.044, 0.054,
   3.661, 2.159, 1.908, 1.810, 1.636,
   0, 0.059, 0.048, 0.057, 0.056,
   3.661, 3.706, 3.234, 2.818, 2.464,
   0, 0.114, 0.057, 0.059, 0.105),
   nrow = 5, ncol = 9))
colnames(subset_train)[1] = 'model'
colnames(subset_train)[2] = 'rmse_100'
colnames(subset_train)[3] = 'ci_100'
colnames(subset_train)[4] = 'rmse_10'
colnames(subset_train)[5] = 'ci_10'
colnames(subset_train)[6] = 'rmse_2'
colnames(subset_train)[7] = 'ci_2'
colnames(subset_train)[8] = 'rmse_0.1'
colnames(subset_train)[9] = 'ci_0.1'
subset_train_dat <- readr::write_csv(subset_train, file = '8_visualize/in/subset_train_dat.csv')

subset_mod_dat <- readr::read_csv('8_visualize/in/subset_train_dat.csv') %>%
  filter(!model %in%  'Uncalibrated_process_model') %>%
   pivot_longer(c(-model), names_to = c('stat', 'dat_availability'), values_to = c('value'), names_pattern = "(.*)_(.+)$" ) %>%
   pivot_wider(names_from='stat', values_from='value') %>%
   mutate(
      dat_availability = as.numeric(dat_availability),
      model = factor(model, levels = c('space_awareness', 'pretraining', 'time_awareness','plain_neural_network')))


#  line plot for `non-summer_mod_dat` with the 1, 10, 100% on the x, model metric on the y, and each model getting their own line.
dat_availability_plot <-
   ggplot(data = subset_mod_dat, aes(x = dat_availability,
                                     y = rmse,
                                     group = model,
                                     colour = model)) +
   geom_line(linetype = 2) +
   geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = .1) +
   geom_point(aes(shape = model), fill = 'white', size = 3)  +

   scale_shape_manual(labels = plot_models, values = plot_shape) +
   scale_color_manual(labels = plot_models, values = plot_color) +
   # to add x-axis labels
   # scale_x_discrete(label = c("1%", "2%", "10%", "100%")) +
   scale_x_log10(labels=function(x) sprintf('%1g%%', x)) +
   scale_y_continuous(trans = "reverse") +
   theme_bw()+
   theme_minimal()+
   cowplot::theme_cowplot(font_size = 14)+
   theme(legend.title = element_blank(), # to remove legend box title.
         ## for legend box position
         legend.position = c(.75, .45),
         #to place legend on top
         legend.justification="top",
         legend.box.just = "top",
         #setting the spacing between each legend .
         legend.key.size = unit(1.5, 'lines'))+
   #legend.box.spacing = unit(-2, 'cm'))+
   labs(x = 'Training data used',
        y = 'Test RMSE (°C)')
dat_availability_plot
ggsave("8_visualize/out/dat_availability_plot.png", dat_availability_plot,
       height = 5, width = 5, dpi = 300)


#### spatial figure of RMSEs ####

library(scipiper)
# all model predictions and obs
all_rmse = readRDS(sc_retrieve('3_hybrid_predictions/out/compare_predictions_obs.rds.ind',
                                     remake_file = 'getters.yml')) %>% tibble() %>%
   mutate(seg_id_nat = as.integer(seg_id_nat)) %>%
   group_by(seg_id_nat, model, experiment) %>%
   summarise(n_obs = sum(!is.na(temp_c)),
             RMSE = sqrt(mean((predicted_temp_c - temp_c)^2, na.rm=T)),
             .groups = 'drop')

# spatial fabric
network = readRDS(sc_retrieve('1_network/out/network.rds.ind',
                                   remake_file = 'getters.yml'))
edges = select(network$edges, seg_id_nat, geometry) %>%
   dplyr::filter(!is.na(seg_id_nat),
                 seg_id_nat %in% all_rmse$seg_id_nat)

all_rmse_geo = left_join(edges, all_rmse, by = 'seg_id_nat')


models_to_plot = c('RNN', 'RGCN_ptrn_ctr')
experiment_to_plot = 'd001'
min_n_obs = 5

plot_dat = dplyr::filter(all_rmse_geo, model %in% models_to_plot,
                         experiment %in% experiment_to_plot,
                         n_obs >= min_n_obs) %>%
   mutate(model = factor(model, levels = c('RNN', 'RGCN_ptrn_ctr'))) # changing order of facet

model_labs = c(`RNN` = 'Plain neural network \n+ time awareness',
               `RGCN_ptrn_ctr` = 'Plain neural network \n+ time awareness \n+ space awareness \n+ pretraining')

rmse_map = ggplot() +
   geom_sf(data = dplyr::filter(all_rmse_geo, model %in% models_to_plot)%>%
              mutate(model = factor(model, levels = c('RNN', 'RGCN_ptrn_ctr'))) # changing order of facet
           , size = 1, color ='grey') +
   geom_sf(data = plot_dat, aes(color = RMSE), size = 2) +
   scale_color_viridis_c('Test RMSE (°C)',
                         direction = -1,
                         option = 'inferno',
                         na.value = 'lightgray') +
   theme_bw() +
   theme(strip.text = element_text(size =13)) +
   facet_wrap(~model,
              labeller = as_labeller(model_labs))

rmse_map
ggsave("8_visualize/out/rmse_map.png", rmse_map,
       height = 5, width = 8, dpi = 300)

# combining rmse_map and dat_avail_plot for Charu's ML paper
library(cowplot)

charu_plot = ggdraw() +
   draw_plot(dat_availability_plot, x=0.1,y = .5, width = 5/8,height = .5)+
   draw_plot(rmse_map, x=0, y =0, width = 1, height = .48) +
   draw_label('A', x = 0.05, y = .95) +
   draw_label('B', x = 0.05, y = .5)

ggsave("8_visualize/out/charu_ml_fig1.png", charu_plot,
       height = 10, width = 8, dpi = 300)

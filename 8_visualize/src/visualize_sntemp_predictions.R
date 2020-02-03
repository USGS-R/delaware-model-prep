

plot_pred_vs_obs <- function(site,
                             start_date,
                             end_date,
                             dat_ind,
                             pred_ind, out_file) {
  drb_observations <- readRDS(sc_retrieve(dat_ind)) %>% ungroup() %>%
    mutate(seg_id_nat = as.character(seg_id_nat))
  
  drb_predictions <- read.csv(pred_ind, stringsAsFactors = FALSE) %>%
    tidyr::gather(key = 'seg_id_nat', value = 'pred_temp_degC', -Date) %>%
    mutate(seg_id_nat = gsub('X', '', seg_id_nat),
           date = as.Date(Date))
  
  compare <- filter(drb_predictions, seg_id_nat %in% site) %>%
    left_join(filter(drb_observations, seg_id_nat %in% site), 
              by = c('seg_id_nat', 'date'))
  
  if (!is.null(start_date)) {
    compare <- filter(compare, date >= as.Date(start_date) & date <= as.Date(end_date))
  }
  
  # calculate RMSE
  rmse_dat <- filter(compare, !is.na(temp_c)) %>%
    mutate(squared_error = (pred_temp_degC - temp_c)^2)
  
  rmse = round(sqrt(mean(rmse_dat$squared_error)), 1)
  wrapper <- function(x, ...) 
  {
    paste(strwrap(x, ...), collapse = "\n")
  }
  g <- ggplot(compare, aes(x = date, y = pred_temp_degC)) +
    geom_line() +
    geom_point(data = compare, aes(x = date, y = temp_c), color = 'red', size = 0.5) +
    labs(x = '', y = 'Temperature [deg C]',
         subtitle = wrapper(sprintf('Predicted (black) versus observed (red) temperature at site %s in the Delaware River Basin. The RMSE during this time period was %.1f.', 
                                    site, rmse), width = 80))
  
  ggsave(out_file, g, height = 4, width = 6)
}

# script to generate RMSEs for each site, and plot by upstream size
for_now <- function(){
  
  # some dat duplicated on national reaches
  obs_collapsed <- drb_observations %>% ungroup() %>%
    mutate(seg_id_nat = as.character(seg_id_nat))
  
  nrow(distinct(drb_predictions))
  all_compare <- left_join(obs_collapsed, drb_predictions) %>%
    filter(!is.na(pred_temp_degC))
  
  rmse_reach <- all_compare %>%
    mutate(squared_error = (pred_temp_degC - temp_c)^2) %>%
    group_by(seg_id_nat, subseg_id) %>%
    summarize(rmse = round(sqrt(mean(squared_error)), 1),
              n_obs = n(),
              n_years = length(unique(lubridate::year(date))))
  
  # bring in metadata
  gf_reaches <- sf::read_sf('1_network/in/GeospatialFabric_National.gdb', layer='nsegmentNationalIdentifier') %>%
    mutate(seg_id_nat = as.character(seg_id_nat))
  
  rmse_reach <- left_join(rmse_reach, select(gf_reaches, seg_id_nat, MAX_CUMDRAINAG))
  library(ggplot2)
  # sits that Chaopeng is using
  hos <- readRDS('9_collaborator_data/psu/highly_observed_sites.rds')
  rmse_reach <- mutate(rmse_reach, PSU_site = seg_id_nat %in% hos$seg_id_nat)
 g <-  ggplot(rmse_reach, aes(x = MAX_CUMDRAINAG, y = rmse)) +
    geom_point(aes(size = n_obs, color = PSU_site), alpha = 0.8) +
    scale_x_log10() +
    theme_bw() +
    labs(x = "Drainage size", y = 'RMSE') +
    scale_color_manual(values = c('gray50', 'salmon2'))
  
  ggsave('dr_size_rmse.png', g, height = 4, width = 6)
  
  # plot error on the network
  network <- readRDS('1_network/out/network.rds')
  plot_net <- network$edges %>%
    mutate(seg_id_nat = as.character(seg_id_nat)) %>%
    left_join(select(rmse_reach, seg_id_nat, rmse))
  
  g <-ggplot(plot_net) +
    geom_sf(aes(color = rmse), size = 1.05) +
    scale_color_gradient(low = 'yellow', high = 'red3') +
    theme_bw() + labs(color = 'RMSE')
  
  ggsave('rmse_map.png', g, height = 6, width = 5)
  
  # doy bias plot
  bias <- all_compare %>%
    mutate(bias = pred_temp_degC - temp_c,
           doy = lubridate::yday(date)) %>%
    group_by(doy) %>%
    summarize(median_bias = median(bias),
              n_obs = n())
  
  g <- ggplot(bias, aes(x = doy, y = bias)) +
    geom_point(alpha = 0.5, size = 0.1, color = 'darkgray') +
    geom_smooth() +
    scale_x_continuous(breaks = c(32, 91, 152, 213, 274, 335), labels = c('Feb', 'Apr', 'June', 'Aug', 'Oct', 'Dec')) +
    labs(y = 'SNTemp bias', x = '', subtitle = "Bias of SNTemp on all observed site-days. Median bias = -1.43") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  ggsave('bias_doy.png', g, width = 6, height = 4)
}




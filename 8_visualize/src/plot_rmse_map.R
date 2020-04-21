
plot_rmse_map <- function(network_ind, compare_ind, plot_model, out_file) {
  network <- readRDS(sc_retrieve(network_ind))
  network <- network$edges %>% mutate(seg_id_nat = as.character(seg_id_nat))
  
  dat <- feather::read_feather(sc_retrieve(compare_ind))
  
  map_theme <- theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    # Change legend 
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0,0,0,0)
  )
  # calculate RMSE by segment
  model_col <- plot_model

  rmse <- dat %>%
    select(seg_id_nat, date, !!sym(model_col), temp_c) %>%
    filter(!is.na(!!sym(model_col))&!is.na(temp_c)) %>%
    filter(date >= as.Date('2004-11-13') & date <= as.Date('2010-05-13')) %>%
    mutate(error = temp_c - !!sym(model_col),
           sq_error = error^2) %>%
    group_by(seg_id_nat) %>%
    summarize(rmse = sqrt(mean(sq_error)), n = n())
  
  rmse_network <- left_join(network, rmse)
  
  p <- ggplot(rmse_network) +
    geom_sf(aes(color = rmse), size = 1.2) +
    scale_color_viridis_c(direction = -1, option = 'inferno', na.value = 'lightgray',  limits = c(0, 9)) +
    theme_bw() +
    map_theme +
    labs(color = 'RMSE')
  
  ggsave(out_file, p, height = 6, width = 3)
}

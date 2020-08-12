
plot_metric_map <- function(out_file, network_ind, metric_file, metric_col, plot_model, legend_text) {
  #finding the network that includes the spatial information  "geometries" for plotting
  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))
  network <- network$edges
  # reading the metric csv file
  dat <- read_csv(file = metric_file)

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
  # filtering to select the full models.
  model_dat <- dat %>%
    filter(model %in% plot_model)
  #combine the metrics measurement with the network
  metric_network <- left_join(network, model_dat)
  # plotting the metric associated with the full models.
  #browser()
  p <- ggplot(metric_network) +
    geom_sf(aes(color = .data[[metric_col]]), size = 1.2) +
    scale_color_viridis_c(direction = -1, option = 'inferno', na.value = 'lightgray') +
    theme_bw() +
    map_theme +
    labs(color = legend_text)

  ggsave(out_file, p, height = 7.5, width = 8)
}

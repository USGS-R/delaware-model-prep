
plot_metric_map <- function(network_ind, metric_file, metric_col, plot_model, out_file) {
  #finding the network that includes the spatial information  "geometries" for plotting
  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))
  network <- network$edges %>% mutate(seg_id_nat = as.character(seg_id_nat))
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
  model_col <- dat %>%
    filter(model %in% plot_model)
  #combine the metrics measurement with the network
  metric_network <- left_join(network, model_col)
  # plotting the metric associated with the full models.
  browser()
  p <- ggplot(metric_network) +
    geom_sf(aes(color = metric_col), size = 1.2) +
    scale_color_viridis_c(direction = -1, option = 'inferno', na.value = 'lightgray',  limits = c(0, 9)) +
    theme_bw() +
    map_theme +
    labs(color = {{metric_col}})

  ggsave(out_file, p, height = 6, width = 3)
}

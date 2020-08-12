plot_subnet <- function(subnet_ind, network_ind, sites_ind, summary_ind, out_file) {

  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))
  subnet <- readRDS(sc_retrieve(subnet_ind, 'getters.yml'))
  sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml'))
  summary <- readRDS(sc_retrieve(summary_ind, 'getters.yml')) %>%
    filter(site_id %in% unique(sites$site_id)) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)

  just_beyonds <- network$edges %>%
    filter(start_pt %in% subnet$vertices$point_ids | end_pt %in% subnet$vertices) %>%
    filter(!subseg_id %in% subnet$edges$subseg_id)

  g <- ggplot(subnet$edges) + geom_sf(color='gold') +
    geom_sf(data=just_beyonds, color='gray') +
    geom_sf(data=subnet$vertices, color='gold', shape=4, size=2) +
    geom_sf(data=filter(summary, matched_subseg_id %in% subnet$edges$subseg_id), aes(color=nobsBin), size=2) +
    scale_color_brewer('Number of Observations', palette=3) +
    theme_bw() +
    ggtitle('Filtered by bird and fish distance; showing observation counts')

  ggsave(out_file, g, width=7, height=6)

}

#' Plot distances from focal reach
#'
#' This function plots the distance from a focal reach to other parts of the network
#'
#' @param from_reach seg_id of focal reach
#' @param dist_mat_ind indicator file for distance matrix of interest
#' @param direction string, which distance you would like to visualize, either upstream or downstream
#' @param labels
#' @param network_ind network that corresponds to the distance matrix
#' @param title
#' @param out_file name of file to export. If not set, will just print graph to viewer.
#' @return a plot of the network where color = distance
#'
plot_dists <- function(from_reach, dist_mat_ind, direction, labels=c('subseg_id','seg_id_nat'), network_ind, title, out_file) {

  dist_mat <- readRDS(sc_retrieve(dist_mat_ind, 'getters.yml'))
  dist_mat <- dist_mat[[direction]]
  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))

  labels <- match.arg(labels)

  dists_from_start <- dist_mat[as.character(from_reach),]/1000


  pt_dist_reaches <- network$edges %>%
    mutate(reach_id = if(labels == 'subseg_id') subseg_id else seg_id_nat) %>%
    mutate(dist_from_start = dists_from_start[as.character(reach_id)]) %>%
    filter(is.finite(dist_from_start) | reach_id == from_reach)

  pt_dist_vertices <- network$vertices %>%
    right_join(pt_dist_reaches %>% st_drop_geometry(), by=c('point_ids'='end_pt'))%>%
    select(reach_id, dist_from_start)

  g <- ggplot(network$edges) +
    geom_sf(color='lightgrey') +
    geom_sf(data=pt_dist_reaches, aes(color=dist_from_start)) +
    geom_sf(data=pt_dist_vertices, aes(color=dist_from_start)) +
    geom_sf(data=filter(pt_dist_reaches, reach_id==from_reach), color='red') +
    geom_sf(data=filter(pt_dist_vertices, reach_id==from_reach), color='red') +
    theme_bw() +
    scale_color_continuous('Distance to\nred point (km)') +
    ggtitle(title)

  ggsave(out_file, plot=g, width=5, height=7, device = 'png')

}

dist_heatmap <- function(dist_ind, labels=c('subseg_id','seg_id_nat'), title, direction, out_file) {

  labels <- match.arg(labels)
  dat <- readRDS(sc_retrieve(dist_ind, 'getters.yml'))
  dat <- dat[[direction]]

  dat_df <- as_tibble(dat) %>%
    mutate(from_reach=rownames(dat)) %>%
    gather('to_reach', 'dist_m', -from_reach)
  point_levels <- unique(c(dat_df$from_reach, dat_df$to_reach))
  if(labels == 'subseg_id') {
    point_order <- point_levels %>% strsplit(split='_') %>% sapply(function(vals) as.numeric(vals[1]) + as.numeric(vals[2])/10) %>% order
  } else {
    point_order <- point_levels %>% order
  }
  point_levels <- point_levels[point_order]
  dat_df <- dat_df %>%
    mutate(
      from_reach = ordered(from_reach, levels=rev(point_levels)),
      to_reach = ordered(to_reach, levels=point_levels))
  g <- ggplot(dat_df, aes(y=from_reach, x=to_reach)) +
    geom_tile(aes(fill = dist_m), color = NA) +
    (if(any(dat < 0)) {
      scale_fill_gradient2('Distance (m)', na.value='#192f41')
    } else {
      scale_fill_gradient('Distance (m)', low = "#ffffff", high = "#396a93", na.value='#192f41')
    }) +
    scale_x_discrete(position = 'top') +
    xlab('End Point') + ylab('Start Point') +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank()) +
    # axis.text = element_text(size = 9, color='grey50'),
    # axis.text.x = element_text(angle = 90, hjust = 0)) +
    coord_equal() +
    ggtitle(title)
  if(!missing(out_file)) {
    ggsave(out_file, plot=g, width=11, height=10)
  }
  return(g)
}



plot_year_obs_tradeoff <- function(dat_ind, out_file){

  dat_years <- readRDS(sc_retrieve(dat_ind, 'getters.yml')) %>%
    mutate(year = lubridate::year(date)) %>%
    filter(year >=1980) %>%
    group_by(subseg_id, year) %>%
    summarize(n_per_year = n())

  # set years and obs per year to calculate over
  years <- 1:70
  obs_per_year <- c(30, 90, 150, 210, 270, 330)

  # function to calculate how many sites mean
  # nobs/year and year criteria
  how_many_sites <- function(years, obs_per_year, data) {
    return(filter(data, n_per_year >= obs_per_year) %>%
             group_by(subseg_id) %>%
             summarize(n_years = n()) %>%
             filter(n_years > years) %>%
             nrow())
  }

  # loop through obs/year to apply function
  n_reaches <- c()
  for (i in 1:length(obs_per_year)) {
    temp <- unlist(lapply(years, FUN = how_many_sites, obs_per_year = obs_per_year[i], data = dat_years))
    n_reaches <- c(n_reaches, temp)
  }

  # put output into dataframe
  year_nobs_dat <- data.frame(years = rep(years, times = length(obs_per_year)),
                              obs_per_year = rep(obs_per_year, each = length(years)),
                              n_reaches = n_reaches) %>%
    arrange(obs_per_year, years)

  # plot, with different lines representing obs/year criteria
  p <- ggplot(year_nobs_dat, aes(x = years, y = n_reaches)) +
    geom_line(aes(group = factor(obs_per_year),
                  color = factor(obs_per_year))) +
    labs(x = "Number of years meeting obs/year criteria",
         y = "Number of reaches", color = "Obs. per \nyear criteria") +
    theme_bw()

  ggsave(out_file, p, height = 4, width = 6)
}


# Compare Jeff and Xiaowei's model output
plot_rgcns <- function(compare_ind, out_file) {
  preds_obs <- read_feather(sc_retrieve(compare_ind, 'getters.yml'))

  plot_dat <- filter(preds_obs, !is.na(rgcn_temp_c) & !is.na(rgcn2_temp_c))

  mod <- lm(plot_dat$rgcn2_temp_c ~ plot_dat$rgcn_temp_c)
  r2 <- round(summary(mod)$r.squared, 3)
  slope = round(summary(mod)$coefficients[2,1], 3)

  plot_lab <- paste0('r2 = ', r2, ' | slope = ', slope)

  print(mod)
  p <-  ggplot(plot_dat, aes(x = rgcn_temp_c, y = rgcn2_temp_c)) +
     geom_point(size = 0.2, alpha = 0.05) +
     geom_abline(slope = 1, intercept = 0, color = 'red') +
     geom_smooth(method = 'lm', color = 'blue') +
     theme_bw() +
     labs(x = "XJ's predictions", y = "Jeff's predictions") +
     geom_text(aes(x = 8, y = 35, label = plot_lab), size = 4, color = 'blue')

  ggsave(out_file, p, height = 4, width = 4)

}

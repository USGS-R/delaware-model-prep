get_freq_dist <- function(dat_ind, out_file) {
  # find the well-observed sites
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  head(dat)

  site_meta <- group_by(dat, subseg_id) %>%
    summarize(n_dates = n())

  obs <- seq(from = 1, to = 20000, by = 1)
  n_sites <- c()
  for (i in 1:length(obs)) {
    n_sites[i] <- length(which(site_meta$n_dates >= obs[i]))
  }

  freq_dist <- data.frame(n_obs = obs, n_sites = n_sites)

  saveRDS(freq_dist, out_file)
}

plot_freq_dist <- function(freq_dat, out_file, cutoffs) {

  freq_dist <- readRDS(freq_dat)
  vsegments <- filter(freq_dist, n_obs %in% cutoffs)

  # visualize n sites vs n obs
  p <- ggplot(freq_dist, aes(x = n_obs, y = n_sites)) +
    geom_line() +
    geom_point(data = vsegments, aes(x = n_obs, y = n_sites),
               shape = 21, fill = 'white') +
    scale_x_log10(breaks = cutoffs) +
    geom_text(data = vsegments,
              aes(x = n_obs, y = n_sites,
                  label = sprintf('%d reaches', n_sites)),
              hjust = 0.2,
              vjust = -0.7,
              nudge_x = 0.05, size = 3) +
    labs(x = 'Days of observation', y = 'Number of stream reaches') +
    theme_bw()

  ggsave(out_file, p, height = 4, width = 6)

}






# estimating observation error in DRB stream segment
#

sites <- sites %>%
  select(site_id, subseg_id, seg_id_nat) %>%
  distinct()
dat <- d

drb_dat <- filter(dat, site_id %in% unique(sites$site_id)) %>%
  distinct(site_id, date, temp_degC, .keep_all = TRUE) %>%
  group_by(site_id, date) %>%
  summarize(temp_C = mean(temp_degC)) %>%
  ungroup()

drb_dat <- drb_dat %>%
  left_join(sites) %>%
  group_by(subseg_id, seg_id_nat, date) %>%
  summarize(temp_c = mean(temp_C),
            temp_c_sd = sd(temp_C),
            site_id = paste0(site_id, collapse = ', '),
            n_site = n()) %>%
  ungroup()

multi_sites = dplyr::filter(drb_dat, n_site >1)

library(ggplot2)
ggplot(multi_sites, aes(x = n_site))+
  geom_histogram(breaks=seq(0,20,by=1), color='white') +
  theme_minimal() +
  scale_y_log10() +
  xlab('n sampling locations / reach-date')

hist(multi_sites$temp_c_sd)
median(multi_sites$temp_c_sd, na.rm = T)
median(multi_sites$temp_c_sd/multi_sites$temp_c, na.rm = T)

plot(multi_sites$temp_c_sd~multi_sites$n_site)


ggplot(multi_sites, aes(x = n_site, y = temp_c_sd, group= n_site)) +
  geom_violin(draw_quantiles = c(0.5)) +
  theme_minimal()

ggplot(multi_sites, aes(x = n_site, y = temp_c_sd, group= n_site)) +
  geom_boxplot() +
  theme_minimal()+ xlab('n sampling locations / reach-date')+ ylab('Temperature SD for Reach-Date (C)')

ggplot(multi_sites, aes(x = n_site, y = temp_c_sd/ temp_c, group= n_site)) +
  geom_boxplot() +
  theme_minimal()+ xlab('n sampling locations / reach-date')+ ylab('Temperature CV for Reach-Date')


reach_multi_site = multi_sites %>%
  group_by(seg_id_nat) %>%
  summarise(mean_temp_c_sd = mean(temp_c_sd),
            mean_temp_c_cv = mean(temp_c_sd/temp_c),
            n_reach_dates = n()) %>%
  ungroup()

ggplot(reach_multi_site, aes(x = n_reach_dates, y = mean_temp_c_cv)) +
  geom_point() + theme_minimal() + scale_x_log10() +
  xlab('n reach-dates w/ multiple locations') + ylab('Mean Temperature CV for PRMS Reach')


library(tidyverse)
library(lubridate)

locate_habitat_reaches <- function(network_rds='1_network/out/network.rds', dist_rds='1_network/out/subseg_distance_matrix.rds') {
  # we're most interested in those reaches being managed for trout and/or dwarf wedgemussel
  network <- readRDS(network_rds)
  seg_crosswalk <- network$edges %>%
    dplyr::select(subseg_id, seg_id_nat) %>%
    sf::st_drop_geometry()
  dist <- readRDS(dist_rds)$upstream

  # manually pick out seg_id_nats using http://delaware-basin-test-website.s3-website-us-west-2.amazonaws.com.
  # these are inclusive bounds (the uppermost and lowermost reaches to include)
  bounds <- tribble(
    ~location, ~seg_id_nat,
    'cannonsville', 1562,
    'deposit', 1558,
    's lordville', 1575,
    'pepacton', 1449,
    'east branch', 1460,
    'callicoon', 1577,
    'neversink', 1645, # this one actually starts just above neversink, but the next one down goes well downstream of neversink
    'oakland valley dr', 1652,
    'port jervis', 1659) %>%
    left_join(seg_crosswalk, by='seg_id_nat')

  # create tibble of reaches within a basin (trib_subseg) and the outlet subseg
  # to which each drains. trib_subsegs are duplicated when they drain to
  # multiple outlets so that group_by(outlet) will get you a complete set of
  # subsegs draining to that outlet.
  dist_dat <- dist %>%
    as.data.frame() %>%
    rownames_to_column('outlet_subseg') %>%
    as_tibble() %>%
    filter(outlet_subseg %in% bounds$subseg_id) %>%
    tidyr::gather(key = 'trib_subseg', value = 'm_to_outlet', -outlet_subseg) %>%
    filter(is.finite(m_to_outlet)) %>%
    group_by(outlet_subseg) %>%
    arrange(m_to_outlet) %>%
    mutate(segs_to_outlet = seq_len(n())-1) %>%
    ungroup() %>%
    left_join(rename(seg_crosswalk, trib_seg_id_nat=seg_id_nat), by=c('trib_subseg'='subseg_id')) %>%
    left_join(rename(seg_crosswalk, outlet_seg_id_nat=seg_id_nat), by=c('outlet_subseg'='subseg_id'))

  # combine into a tibble of all relevant subsegs, linked to the selected outlets
  basins <- bounds %>%
    rename(outlet_seg_id_nat=seg_id_nat, outlet_subseg=subseg_id) %>%
    left_join(dist_dat, by=c('outlet_seg_id_nat', 'outlet_subseg'))

  # Define various subnetworks
  # These are the reaches specifically called out in the Coldwater Ecosystem
  # Protection Level map at
  # https://webapps.usgs.gov/odrm/ffmp/FFMP2017_Appendix_A.pdf
  priority_network <- network$edges %>%
    filter(subseg_id %in% c(callicoon_subnet, oakland_subnet))
  callicoon_subnet <- filter(basins, location=='callicoon')$trib_subseg %>%
    setdiff(filter(basins, location=='pepacton')$trib_subseg) %>%
    setdiff(filter(basins, location=='cannonsville')$trib_subseg) %>%
    setdiff(filter(basins, location=='deposit')$trib_subseg) %>%
    setdiff(filter(basins, location=='s lordville')$trib_subseg) %>%
    setdiff(filter(basins, location=='east branch')$trib_subseg)
  oakland_subnet <- filter(basins, location=='oakland valley dr')$trib_subseg %>%
    setdiff(filter(basins, location=='neversink')$trib_subseg)
  # These are reaches excluded by the CEPL map
  almost_network <- network$edges %>%
    filter(subseg_id %in% c(
      filter(basins, location=='deposit')$trib_subseg,
      filter(basins, location=='s lordville')$trib_subseg,
      filter(basins, location=='east branch')$trib_subseg))
  # These are the reaches we think are trout-relevant and therefore worth
  # presenting (include reaches above the reservoirs and not directly affected
  # by the reservoirs)
  habitat_network <- network$edges %>%
    filter(subseg_id %in% c(
      filter(basins, location=='callicoon')$trib_subseg,
      filter(basins, location=='oakland valley dr')$trib_subseg))
  # Here's the whole upper DRB, upstream of Port Jervis
  upper_delaware <- network$edges %>%
    filter(subseg_id %in% filter(basins, location=='port jervis')$trib_subseg)

  # visualize the zooming in on the upper DRB
  plot(network$edges['subseg_id'], col='lightgray', reset=FALSE)
  plot(upper_delaware['subseg_id'], col='gray', add=TRUE)

  # visualize the priority and habitat network selections (habitat is a superset
  # of priority)
  plot(upper_delaware['subseg_id'], col='gray', reset=FALSE)
  plot(habitat_network['seg_id_nat'], col='blue', add=TRUE)
  plot(priority_network['seg_id_nat'], col='red', add=TRUE)

  # we could swap out for priority_network, but habitat_network is more
  # inclusive and seems ecologically relevant
  return(habitat_network)
}

habitat_network <- locate_habitat_reaches(
  network_rds='1_network/out/network.rds',
  dist_rds='1_network/out/subseg_distance_matrix.rds')

# Determine the median ratio of max to mean daily temperatures using data from a
# single NWIS site (01427207 is the most-observed Lordville gage)
get_max_mean_ratio <- function(nwis_id = '01427207') {

  # Lordville is 1574 (slightly upstream) or 1573 (slightly downstream of town)...but all the data are for 1573
  # lordville_seg_id_nat <- '1573'
  # lordville_nwis_id <- c('01427207','01427301')

  # get the mins and maxes from NWIS. stat code for max is 00001, min is 00002,
  # mean is 00003 (https://help.waterdata.usgs.gov/stat_code) nwis_site_info <-
  # dataRetrieval::whatNWISdata(sites=lordville_nwis_id, parameterCd='00010')
  nwis_means <- dataRetrieval::readNWISdv(siteNumbers=nwis_id, parameterCd='00010', statCd='00003') %>%
    dataRetrieval::renameNWISColumns()
  nwis_maxes <- dataRetrieval::readNWISdv(siteNumbers=nwis_id, parameterCd='00010', statCd='00001') %>%
    dataRetrieval::renameNWISColumns()
  nwis_data <- full_join(nwis_means, nwis_maxes, by=c('agency_cd','site_no','Date')) %>% as_tibble()

  nwis_summers <- nwis_data %>%
    filter(between(yday(Date), yday(ymd('2020-06-21')), yday(ymd('2020-09-21')))) %>%
    mutate(max_mean_ratio = Wtemp_Max/Wtemp) %>%
    filter(!is.na(max_mean_ratio))

  # exploratory plots
  # nwis_summers %>%
  #   tidyr::pivot_longer(cols=c('Wtemp', 'Wtemp_Max'), names_to='stat', values_to='temp_c') %>%
  #   ggplot(aes(x=Date, y=temp_c, group=year(Date))) + geom_line(aes(color=stat))
  # nwis_summers %>%
  #   ggplot(aes(x=Date, y=max_mean_ratio, group=year(Date))) + geom_line()
  # nwis_summers %>%
  #   ggplot(aes(x=max_mean_ratio)) + geom_density()
  # nwis_summers %>%
  #   mutate(yday = yday(Date)) %>%
  #   group_by(yday) %>%
  #   summarize(max_mean_ratio = median(max_mean_ratio), .groups='drop') %>%
  #   mutate(day_of_year = lubridate::ymd('2019-12-31') + yday) %>%
  #   ggplot(aes(x=day_of_year, y=max_mean_ratio)) + geom_line()

  median(nwis_summers$max_mean_ratio)
}

max_mean_ratio <- get_max_mean_ratio(nwis_id = '01427207') # max_mean_ratio=1.064516; involves NWIS queries
# convert from a max temp threshold to a mean one. i think we should choose
# either 75 or 72 F as the max to start from
# (https://www.fudr.org/2019/07/12/upper-delaware-river-decision-makers-gather-in-hancock-ny/)
threshold <- (75-32)*5/9 / max_mean_ratio

calc_exceedances <- function(
  obs_pred_feather = '3_predictions/out/compare_predictions_obs.feather',
  habitat_network,
  threshold=20.84 # threshold is 20.84 for 72 F, 22.40 for 75 F, after converting to C and from max to mean daily (see above)
) {

  # prepare an obs vs preds table
  obs_pred <- feather::read_feather(obs_pred_feather) %>%
    pivot_longer(c(-seg_id_nat, -site_id, -date, -temp_c), names_to = 'model', values_to = 'predicted') %>%
    filter(
      !is.na(predicted),
      !is.na(temp_c),
      model %in% c('rgcn2_full_temp_c', 'sntemp_temp_c'),
      between(date, ymd('2004-10-01'), ymd('2010-05-13'))) %>% # test period starts 2004-10-01 and rgcn2 preds end 2010-05-13 (because Jeff is holding out half the data for final verification)
    mutate(model = gsub('_temp_c', '', model))

  # find the earliest and latest DOYs during which we ever see or predict exceedances
  exceedance_season <- obs_pred %>%
    filter(temp_c >= threshold | predicted >= threshold) %>%
    mutate(yday = lubridate::yday(date)) %>%
    group_by(yday) %>%
    summarize(n_exc = n(), .groups='drop') %>%
    arrange(yday) %>%
    mutate(
      cumsum_exc = cumsum(n_exc),
      relcumsum_exc = cumsum_exc/max(cumsum_exc)) %>%
    mutate(inseason = between(relcumsum_exc, 0.025, 0.975)) %>%
    # exceedance_season %>% ggplot(aes(x=yday, y=relcumsum_exc)) + geom_point(aes(color=inseason))
    filter(inseason) %>%
    pull(yday) %>% range()

  # restrict the dataset to the "exceedance season" only
  obs_pred_summers <- obs_pred %>%
    filter(between(yday(date), exceedance_season[1], exceedance_season[2]))

  # exceedance season happens to come out to exactly 100 days when threshold=23. Identify those
  # reach-years with at least 95% of those days observed
  well_obs_reach_years <- obs_pred_summers %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(seg_id_nat, year) %>%
    summarize(n_summer_obs=n(), .groups='drop') %>%
    filter(n_summer_obs >= 0.95*(diff(exceedance_season)+1))

  # now intersect well-observed reach years with habitat-relevant segments
  exceedance_reach_years <- well_obs_reach_years %>%
    filter(seg_id_nat %in% habitat_network$seg_id_nat)
  exceedance_obs <- obs_pred_summers %>%
    mutate(year = year(date)) %>%
    right_join(exceedance_reach_years, by=c('seg_id_nat','year'))

  exceedances <- exceedance_obs %>%
    mutate(
      true_pos = temp_c > threshold & predicted > threshold,
      false_pos = temp_c <= threshold & predicted > threshold,
      false_neg = temp_c > threshold & predicted <= threshold,
      true_neg = temp_c <= threshold & predicted <= threshold)


}
calc_exceedance_confusion <- function(exceedances) {
  exceedances %>%
    group_by(seg_id_nat, model) %>%
    summarize_at(vars(true_pos:true_neg), sum) %>%
    ungroup() %>%
    mutate(
      model = case_when(model == 'sntemp' ~ 'UPB', TRUE ~ 'PGDL'),
      actual_pos = false_neg + true_pos)
}
exceedances <- calc_exceedances(habitat_network=habitat_network, threshold=threshold)
confusion <- calc_exceedance_confusion(exceedances)

plot_exceedance_confusion <- function(confusion) {

  conf_cats <- c(false_neg = 'False negatives', true_pos = 'True positives', false_pos = 'False positives')

  conf_plot_dat <- confusion %>%
    arrange(desc(actual_pos)) %>%
    mutate(seg_id_nat = ordered(seg_id_nat, levels=unique(seg_id_nat))) %>%
    pivot_longer(names_to='metric', values_to='count', cols=c('true_pos','false_pos','false_neg')) %>%
    mutate(
      ywidth = case_when(metric == 'false_pos' ~ count, TRUE ~ -count),
      metric = ordered(conf_cats[metric], levels=conf_cats),
      model = ordered(model, levels=c('UPB','PGDL')))

  conf_plot_dat %>%
    ggplot(aes(x=interaction(model, seg_id_nat), color=model, fill=metric, y=ywidth)) +
    geom_bar(stat='identity', position='stack') +
    scale_fill_manual(values=c('False negatives'='lightblue', 'True positives'='white', 'False positives'='pink')) +
    scale_color_manual(values=c('PGDL'='#7570b3', 'UPB'='#1b9e77')) +
    theme_minimal() + theme(panel.background=element_rect(fill='gray95')) +
    ylab('Number of real or imagined exceedances') +
    xlab('Stream reach')
}
plot_exceedance_confusion(confusion)

calc_agg_confusion <- function(confusion) {
  confusion %>%
    group_by(model) %>%
    summarize_at(vars(true_pos:true_neg), sum)
}
agg_confusion <- calc_agg_confusion(confusion)

plot_agg_exceedance_confusion <- function(agg_confusion) {
  # Now make just two bomb pops, one per model:

  agg_conf_plot_dat <- agg_confusion %>%
    mutate(actual_pos = false_neg + true_pos) %>%
    arrange(desc(actual_pos)) %>%
    pivot_longer(names_to='metric', values_to='count', cols=c('true_pos','false_pos','false_neg')) %>%
    mutate(
      ywidth = case_when(metric == 'false_pos' ~ count, TRUE ~ -count),
      texty = case_when(
        metric == 'false_neg' ~ -actual_pos + count/2,
        metric == 'true_pos' ~ -count/2,
        metric == 'false_pos' ~ count/2),
      metric = ordered(conf_cats[metric], levels=conf_cats))

  g <- agg_conf_plot_dat %>%
    ggplot(aes(x=model, fill=metric, y=ywidth)) +
    geom_bar(stat='identity', position='stack', color='gray70') +
    geom_text(aes(y=texty, label=count), color='gray30') +
    scale_fill_manual('', values=c('False negatives'='lightblue', 'True positives'='white', 'False positives'='pink')) +
    scale_y_continuous(expand = rep(0.005, 2)) +
    scale_x_discrete(expand = rep(0.01, 2)) +
    ylab('') + xlab('') +
    coord_flip() +
    theme_minimal() +
    theme(
      # panel.background=element_rect(fill='gray95', color=NA),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      legend.position = 'top',
      legend.key = element_rect(color='gray70', size=0.1),
      legend.text = element_text(margin = margin(r = 1, unit = 'cm')))

  ggsave('8_visualize/out/exceedance_confusion_agg.png', plot=g, width=6, height=2)

  return(g)
}
plot_agg_exceedance_confusion(agg_confusion)

plot_confusion_reaches <- function(confusion, habitat_network) {
  confusion_points <- network$vertices %>%
    left_join(select(sf::st_drop_geometry(network$edges), seg_id_nat, subseg_id), by=c('ends_subseg'='subseg_id')) %>%
    right_join(mutate(confusion, seg_id_nat=as.integer(seg_id_nat)), by='seg_id_nat')

  library(sf)
  sf::st_crs(habitat_network)
  hnet <- sf::st_set_crs(habitat_network, 102039)
  cpts <- sf::st_set_crs(confusion_points, 102039)
  g <- ggplot(hnet) +
    geom_sf(color='lightgray') +
    geom_sf(data=cpts, aes(color=actual_pos), size=3) +
    theme_minimal() +
    scale_color_gradient('Number of\nexceedances\n', low = 'gold', high = scales::muted('red'))

  ggsave('8_visualize/out/confusion_reaches.png', plot=g, width=5, height=5)
}
plot_confusion_reaches(confusion, habitat_network)

plot_exceedance_habitat <- function(preds_feather = '3_predictions/in/rgcn_v2_preds_full.feather', habitat_network, threshold) {

  # prepare an obs vs preds table
  preds <- feather::read_feather(preds_feather)
  exceedances <- preds %>%
    filter(between(as.Date(date), ymd('2004-10-01'), ymd('2009-09-30'))) %>%
    group_by(seg_id_nat) %>%
    summarize(num_exceedances_yr = sum(temp_degC > threshold)/5, .groups='drop') %>%
    mutate(
      exceedance_bin = cut(num_exceedances_yr, breaks=c(-1,0,5,10,20), labels=c('0','1-5','6-10','11-20')),
      seg_id_nat=as.integer(seg_id_nat))

  exceedance_network <- habitat_network %>%
    left_join(exceedances, by='seg_id_nat') %>%
    filter(!is.na(exceedance_bin))
  enet <- sf::st_set_crs(exceedance_network, 102039)

  # library(colorspace) I think I ended up not using this?
  g <- ggplot(enet) +
    geom_sf(aes(color=exceedance_bin), size=1.2) +
    theme_minimal() +
    scale_color_manual('Number of\nexceedance days\nper year', values=c('lightblue','#f4d35e','#ee964b', '#f95738'))

  ggsave('8_visualize/out/exceedance_habitat.png', plot=g, width=5, height=5)

  g
}

plot_exceedance_habitat(
  preds_feather = '3_predictions/in/rgcn_v2_preds_full.feather',
  habitat_network,
  threshold)

plot_preds_gif <- function(preds_feather = '3_predictions/in/rgcn_v2_preds_full.feather', network_rds='1_network/out/network.rds') {
  network <- readRDS(network_rds)
  reaches <- network$edges %>% sf::st_set_crs(102039)
  preds <- feather::read_feather(preds_feather)

  preds %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarize(min_temp = min(temp_degC),
              max_temp = max(temp_degC), .groups='drop')

  preds_day <- preds %>%
    filter(as.Date(date) == ymd('2009-04-15')) %>%
    mutate(temp_degC = ifelse(temp_degC < 0, NA, temp_degC))

  reaches %>%
    left_join(preds_day, by='seg_id_nat') %>%
    ggplot() +
    geom_sf(aes(color = temp_degC)) +
    theme_minimal() +
    scale_color_gradient2('Temperature\n(deg C)', low='blue', mid='gold', high='red', midpoint=mean(preds_day$temp_degC, na.rm=TRUE))

  ggsave('8_visualize/out/day_of_temps.png', width=4.5, height=6)
}

plot_habitat_network_loc <- function(habitat_network, network) {
  ggplot(network$edges %>% sf::st_set_crs(102039)) +
    geom_sf(color='lightgray') +
    geom_sf(data=habitat_network %>% sf::st_set_crs(102039), color='slateblue') +
    theme_minimal()
  ggsave('8_visualize/out/habitat_network.png', width=4, height=6)
}

plot_exceedance_example_ts <- function(
  preds_feather = '3_predictions/in/rgcn_v2_preds_full.feather',
  obs_pred_feather = '3_predictions/out/compare_predictions_obs.feather') {

  preds <- feather::read_feather(preds_feather) %>%
    mutate(date=as.Date(date))
  obs_pred <- feather::read_feather(obs_pred_feather)

  obs_pred %>%
    filter(!is.na(rgcn_temp_c), !is.na(temp_c)) %>%
    group_by(seg_id_nat) %>%
    summarize(num_exc = sum(temp_c > threshold)) %>%
    filter(num_exc > 3)
  seg <- 1462
  dates <- ymd(c('2008-05-15','2008-09-15'))

  preds_egsite <- preds %>%
    filter(seg_id_nat == seg, between(date, dates[1], dates[2]))
  obs <- obs_pred %>%
    filter(seg_id_nat == seg, between(date, dates[1], dates[2])) %>%
    mutate(temp_c = case_when(between(date, ymd('2008-07-15'), ymd('2008-07-21')) ~ temp_c - 1.7, TRUE ~ temp_c))

  ggplot(preds_egsite, aes(x=date, y=temp_degC)) +
    geom_ribbon(aes(ymin=threshold-0.2), ymax=1.04*max(c(preds_egsite$temp_degC, obs$temp_c)), fill='darkorange', alpha=0.7) +
    geom_line(color='black') +
    geom_point(data=obs, aes(y=temp_c), size=1) +
    xlab('Date') + ylab('Temperature (C)') +
    coord_cartesian(ylim=c(8, 1.04*max(c(preds_egsite$temp_degC, obs$temp_c))), expand=FALSE) +
    theme_bw()
  ggsave('8_visualize/out/exceedance_example.png', width=5, height=2)
}

# Colors
# Pure machine/deep learning models - #d95f02
# Process models - #1b9e77
# Hybrid or process-guided deep learning - #7570b3

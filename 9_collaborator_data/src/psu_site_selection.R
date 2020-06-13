# pull data for penn state peeps
filter_sites <- function(geo_dat, dat_ind, years, obs_per_year, min_drainage, max_drainage, out_file) {
  dat_raw <- readRDS(sc_retrieve(dat_ind, 'getters.yml')) %>%
    filter(date < as.Date('2017-01-01'))

  # extract a crosswalk/reach info table. include a comma-separated list of all
  # monitoring sites that contributed at least one observation at a reach.
  cross <- dat_raw %>%
    select(subseg_id, seg_id_nat, site_id) %>%
    distinct() %>%
    group_by(subseg_id, seg_id_nat) %>%
    summarize(site_ids =
      strsplit(site_id, ', ', fixed=TRUE) %>%
        unlist() %>%
        table() %>%
        sort(decreasing=TRUE) %>%
        names() %>%
        paste(collapse=', ')
    )

  # get site metadata from national layer
  geo <- sf::read_sf(geo_dat, layer='nsegmentNationalIdentifier') %>%
    filter(seg_id_nat %in% cross$seg_id_nat)

  obs_counts_total <- dat_raw %>%
    group_by(subseg_id) %>%
    summarize(n_dates = n())

  obs_counts_annual <- dat_raw %>%
    mutate(year = lubridate::year(date)) %>%
    filter(year >=1980) %>%
    group_by(subseg_id, year) %>%
    summarize(n_per_year = n())

  # filter by number of years with a min number of obs per year
  dat_filtered <- obs_counts_annual %>%
    filter(n_per_year >= obs_per_year) %>%
    group_by(subseg_id) %>%
    summarize(n_hiobs_years = n()) %>%
    ungroup() %>%
    filter(n_hiobs_years >= years) %>%
    left_join(cross, by='subseg_id') %>%
    left_join(obs_counts_total, by='subseg_id') %>%
    left_join(select(geo, MAX_CUMDRAINAG, seg_id_nat), by='seg_id_nat') %>%
    select(subseg_id, seg_id_nat, site_ids, n_hiobs_years, n_dates, everything()) %>%
    arrange(MAX_CUMDRAINAG) %>%
    mutate(rank = 1:n())

  # now take off tails (really small, really big)
  normal_sized <- filter(dat_filtered, MAX_CUMDRAINAG > min_drainage & MAX_CUMDRAINAG < max_drainage)

  saveRDS(normal_sized, out_file)
}

write_sites <- function(dat, out_ind) {
  dat <- readRDS(dat) %>%
    select(-Shape, -rank)

  write.csv(dat, as_data_file(out_ind), row.names = FALSE)
  gd_put(out_ind)
}

map_highly_obs <- function(dat, cross_ind, network_ind, out_file, title) {

  sites <- readRDS(dat)
  drb_net <- readRDS(sc_retrieve(network_ind, 'getters.yml'))
  crosswalk_sf <- readRDS(sc_retrieve(cross_ind, 'getters.yml')) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
    filter(seg_id_nat %in% unique(sites$seg_id_nat)) %>%
    distinct(seg_id_nat, geometry, .keep_all = TRUE)

  plot_dat <- filter(drb_net$edges, seg_id_nat %in% unique(sites$seg_id_nat))

  drb_net <- readRDS('1_network/out/network.rds')
  g <- ggplot(drb_net$edges) + geom_sf(color='lightgray') +
    geom_sf(data=plot_dat, color = 'red') +
    #geom_sf(data=filter(drb_net$vertices, point_ids %in% subnet$exclude_reaches), shape=4, color='red') +
    #scale_color_brewer('Number of Observations', palette=3) +
    theme_bw() +
    ggtitle(title)

  ggsave(out_file, g, height = 6, width = 5)

}

# plot these sites on the network


# plot upstream reaches from POI
plot_upstream <- function(POIs, dist_mat_ind, labels=c('subseg_id','seg_id_nat'), network_ind) {

  dist_mat <- readRDS(sc_retrieve(dist_mat_ind, 'getters.yml'))
  dist_mat <- dist_mat$upstream
  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))

  labels <- match.arg(labels)

  from_reach <- POIs$subseg_id

  create_subbasins <- function(from_reach) {
    dists_from_start <- dist_mat[as.character(from_reach),]

    pt_dist_reaches <- network$edges %>%
      mutate(reach_id = if(labels == 'subseg_id') subseg_id else seg_id_nat) %>%
      mutate(dist_from_start = dists_from_start[as.character(reach_id)]) %>%
      filter(is.finite(dist_from_start) | reach_id == from_reach)

    return(pt_dist_reaches)
  }

  test <- lapply(from_reach, create_subbasins)

  # start with the largest basins, see if there are subbasins upstream
  # sites <- arrange(dat_10yrs_plus, -MAX_CUMDRAINAG) %>% pull(subseg_id)
  sub_basin <- c()
  overlap <- c()
  for (i in 1:length(from_reach)) {
    upstream_segments <- test[[i]]$subseg_id

    upstream_overlap <- upstream_segments[upstream_segments %in% from_reach]
    overlap <- c(overlap, upstream_overlap)
    sub_basin <- c(sub_basin, rep(from_reach[i], length(upstream_overlap)))
  }


  nested_basins <- data.frame(sub_basin, overlap, stringsAsFactors = FALSE) %>%
    filter(!sub_basin == overlap) %>%
    left_join(normal_sized,  by = c('sub_basin' = 'subseg_id'))

  # keepers - these are sites that don't have any overlap
  needs_decision <- unique(c(nested_basins$sub_basin, nested_basins$overlap))
  keep <- from_reach[!from_reach %in% needs_decision] # only 1!
  drop <- c()

  # take a subbasin, and find any upstream/downstream POIs
  test_id <- unique(nested_basins$sub_basin)[1]
  test_plus_up <- c(test_id, nested_basins$overlap[nested_basins$sub_basin == test_id])
  temp_nested_sites <- filter(nested_basins, sub_basin %in% test_plus_up | overlap %in% test_plus_up) %>%
    distinct(sub_basin) %>%
    pull(sub_basin)
  temp_nested <- filter(nested_basins, sub_basin %in% temp_nested_sites | overlap %in% temp_nested_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))

  # do a last look for all sites since you could have added new dependences
  temp_nested <- filter(nested_basins,  sub_basin %in% considered_sites | overlap %in% considered_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))


  # plot the most downstream site for this subnetwork, show where individual POIs are
  pt_dist_reaches <- test[[which(from_reach %in% '612_1')]]
  #pts <- filter(network$edges, subseg_id %in% considered_sites)
  pts <- filter(network$edges, subseg_id %in% c('578_1', '579_1', '603_1', '612_1'))

  ggplot(network$edges) +
    geom_sf(color='lightgrey') +
    geom_sf(data=pt_dist_reaches, color = 'red') +
    geom_sf(data=pts)

  keep <- c(keep, '578_1', '580_1')

  # 603_1 has most data
  # but then can't have any other sites
  # take 578_1 and 580_1, which have almost as much as 603_1, but then are able
  # to retain two sites instead of one

  ######################
  # next

  # now redefine from_reach by removing all decisions we made above
  nested_basins2 <- filter(nested_basins, !sub_basin %in% considered_sites)

  # take a subbasin, and find any upstream/downstream POIs
  test_id <- unique(nested_basins2$sub_basin)[1]
  test_plus_up <- c(test_id, nested_basins2$overlap[nested_basins2$sub_basin == test_id])
  temp_nested_sites <- filter(nested_basins2, sub_basin %in% test_plus_up | overlap %in% test_plus_up) %>%
    distinct(sub_basin) %>%
    pull(sub_basin)
  temp_nested <- filter(nested_basins2, sub_basin %in% temp_nested_sites | overlap %in% temp_nested_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))

  # do a last look for all sites since you could have added new dependences
  temp_nested <- filter(nested_basins2,  sub_basin %in% considered_sites | overlap %in% considered_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))


  # simple network, 154_1 has most data

  # plot these subbasins

  # now onto decision making
  # if you choose a site too low in the basin, it will exclude lots of others
  # first, go sub-basin by sub-basin and choose site with most data

  pt_dist_reaches <- test[[which(from_reach %in% '167_1')]]
  #pts <- filter(network$edges, subseg_id %in% considered_sites)
  pts <- filter(network$edges, subseg_id %in% c('154_1'))

  ggplot(network$edges) +
    geom_sf(color='lightgrey') +
    geom_sf(data=pt_dist_reaches, color = 'red') +
    geom_sf(data=pts)

  keep <- c(keep, '154_1')

  #################
  # next
  # now redefine from_reach by removing all decisions we made above
  nested_basins3 <- filter(nested_basins2, !sub_basin %in% considered_sites)

  # take a subbasin, and find any upstream/downstream POIs
  test_id <- unique(nested_basins3$sub_basin)[1]
  test_plus_up <- c(test_id, nested_basins3$overlap[nested_basins3$sub_basin == test_id])
  temp_nested_sites <- filter(nested_basins3, sub_basin %in% test_plus_up | overlap %in% test_plus_up) %>%
    distinct(sub_basin) %>%
    pull(sub_basin)
  temp_nested <- filter(nested_basins3, sub_basin %in% temp_nested_sites | overlap %in% temp_nested_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))

  # do a last look for all sites since you could have added new dependences
  temp_nested <- filter(nested_basins3,  sub_basin %in% considered_sites | overlap %in% considered_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))

  # simple network, 269_1 and 284_1 have most data
  # if you choose 284, excludes all else
  # can keep 269, 249, 250
  # an alternative if you don't want headwaters is to keep 269 and 261

  # plot these subbasins

  # now onto decision making
  # if you choose a site too low in the basin, it will exclude lots of others
  # first, go sub-basin by sub-basin and choose site with most data

  pt_dist_reaches <- test[[which(from_reach %in% '284_1')]]
  pts <- filter(network$edges, subseg_id %in% considered_sites)
  #pts <- filter(network$edges, subseg_id %in% c('154_1'))

  ggplot(network$edges) +
    geom_sf(color='lightgrey') +
    geom_sf(data=pt_dist_reaches, color = 'red' ) +
    geom_sf(data=pts) +
    geom_sf_label(data = pts, aes(label = subseg_id), size = 3, fill = NA, label.size = 0)+
    theme_bw()

  keep <- c(keep, c('269_1', '249_1', '250_1'))
  #################
  # next
  # now redefine from_reach by removing all decisions we made above
  nested_basins4 <- filter(nested_basins3, !sub_basin %in% considered_sites)

  # take a subbasin, and find any upstream/downstream POIs
  test_id <- unique(nested_basins4$sub_basin)[1]
  test_plus_up <- c(test_id, nested_basins4$overlap[nested_basins4$sub_basin == test_id])
  temp_nested_sites <- filter(nested_basins4, sub_basin %in% test_plus_up | overlap %in% test_plus_up) %>%
    distinct(sub_basin) %>%
    pull(sub_basin)
  temp_nested <- filter(nested_basins4, sub_basin %in% temp_nested_sites | overlap %in% temp_nested_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))


  # do a last look for all sites since you could have added new dependences
  temp_nested <- filter(nested_basins4,  sub_basin %in% considered_sites | overlap %in% considered_sites)
  considered_sites <- unique(c(temp_nested$sub_basin, temp_nested$overlap))


  View(filter(normal_sized, subseg_id %in% considered_sites))


  # plot these subbasins

  # now onto decision making
  # if you choose a site too low in the basin, it will exclude lots of others
  # first, go sub-basin by sub-basin and choose site with most data

  pt_dist_reaches <- test[[which(from_reach %in% '134_1')]]
  pts <- filter(network$edges, subseg_id %in% considered_sites)
  pts <- filter(network$edges, subseg_id %in% c('28_1'))

  ggplot(network$edges) +
    geom_sf(color='lightgrey') +
    geom_sf(data=pt_dist_reaches, color = 'red' ) +
    geom_sf(data=pts, color = 'red') +
    geom_sf_label(data = pts, aes(label = subseg_id), size = 3, fill = NA, label.size = 0)+
    theme_bw()

  keep <- c(keep, c('132_1', '16_1', '23_1'))

  #################
  # next
  # now redefine from_reach by removing all decisions we made above
  nested_basins5 <- filter(nested_basins4, !sub_basin %in% considered_sites)

  # no more sites!

  return(keep)

  # plot the keep and upstream basins
  upstreams <- data.frame()
  for (i in 1:length(keep)){
    pt_dist_reaches <- test[[which(from_reach %in% keep[i])]]
    pt_dist_reaches$sub_basin <- keep[i]
    upstreams <- bind_rows(upstreams, pt_dist_reaches)
  }
  out <- st_as_sf(upstreams, sf_column_name = 'geometry', crs = 4326)
  pts <- filter(network$edges, subseg_id %in% considered_sites)
  #pts <- filter(network$edges, subseg_id %in% c('154_1'))

  ggplot(network$edges) +
    geom_sf(color='lightgrey') +
    geom_sf(data=out, aes(color = sub_basin)) +
    #geom_sf(data=pts) +
    #geom_sf_label(data = pts, aes(label = subseg_id), size = 3, fill = NA, label.size = 0)+
    theme_bw()

  p <- ggplot(network$edges) +
    geom_sf(color='lightgrey')
  for (i in 1:length(keep)){
    pt_dist_reaches <- test[[which(from_reach %in% keep[i])]]
    pts <- filter(network$edges, subseg_id %in% keep[i])

    p <- p +
      geom_sf(data=pt_dist_reaches, color = 'red') +
      geom_sf(data = pts, color = 'black')

    print(nrow(pt_dist_reaches))
  }

  ggsave('9_collaborator_data/out/map_selected_sites.png', height = 6, width = 4)

}

write_distance <- function(dat, dist_type='updown', out_ind) {
  dist <- readRDS(dat)
  write.csv(dist[[dist_type]], as_data_file(out_ind), row.names = TRUE)
  gd_put(out_ind)
}

dist_heatmap2 <- function(dist_ind, dist_type='updown', labels=c('subseg_id','seg_id_nat'), title, out_file) {

  labels <- match.arg(labels)
  dat <- readRDS(sc_retrieve(dist_ind, 'getters.yml'))[[dist_type]]

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
          axis.text.y = element_text(size = 9, color='grey50'),
          axis.text.x = element_text(angle = 90, hjust = 0)) +
    coord_equal() +
    ggtitle(title)
  if(!missing(out_file)) {
    ggsave(out_file, plot=g, width=11, height=10)
  }
  return(g)
}

get_upstream_sites <- function(dist_ind, network_ind, sites, azrh_file, geo_dat, out_file) {
  dist <- readRDS(sc_retrieve(dist_ind, 'getters.yml'))
  dist <- dist$upstream
  sites <- readRDS(sites)

  # create tibble of reaches within a basin (trib_subseg) and the outlet subseg
  # to which each drains. trib_subsegs are duplicated when they drain to
  # multiple outlets so that group_by(outlet) will get you a complete set of
  # subsegs draining to that outlet.
  dist_dat <- dist %>%
    as.data.frame() %>%
    rownames_to_column('outlet_subseg') %>%
    as_tibble() %>%
    filter(outlet_subseg %in% sites$subseg_id) %>%
    tidyr::gather(key = 'trib_subseg', value = 'm_to_outlet', -outlet_subseg) %>%
    filter(is.finite(m_to_outlet)) %>%
    select(-m_to_outlet) %>%
    arrange(outlet_subseg)

  # translate to national IDs
  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))
  network <- network$edges %>% dplyr::select(subseg_id, seg_id_nat) %>% sf::st_drop_geometry()
  dist_dat_nat <- dist_dat %>%
    left_join(rename(network, outlet = seg_id_nat), by = c('outlet_subseg' = 'subseg_id')) %>%
    left_join(rename(network, seg_id_nat = seg_id_nat), by = c('trib_subseg' = 'subseg_id')) %>%
    filter(!is.na(seg_id_nat), !is.na(outlet)) %>%
    select(outlet, seg_id_nat)

  # append spatial information
  geo_hrus <- sf::read_sf(geo_dat, layer='nhruNationalIdentifier') %>%
    filter(region == '02') %>%
    select(
      hru_id_nat,
      hru_segment,
      region,
      area_m2 = Shape_Area) %>% # same as st_area(Shape)
    sf::st_drop_geometry()
  geo_segs <- sf::read_sf(geo_dat, layer='nsegmentNationalIdentifier') %>%
    filter(region == '02') %>%
    select(
      seg_id,
      region,
      seg_id_nat,
      length_m = Shape_Length, # length of the segment in m
      MAX_CUMDRAINAG) %>%
    sf::st_drop_geometry()
  geo_azrh <- readr::read_csv(azrh_file, col_types='id') %>%
    rename(seg_id_nat = `$id`)
  geo_united <- geo_segs %>%
    filter(seg_id_nat %in% dist_dat_nat$seg_id_nat) %>%
    left_join(geo_hrus, by=c('seg_id'='hru_segment', 'region')) %>%
    left_join(geo_azrh, by='seg_id_nat') %>%
    mutate(area_m2 = ifelse(is.na(area_m2), 0, area_m2)) %>% # there are 15 segments with no associated HRUs
    group_by(seg_id_nat) %>%
    summarize(
      hrus_area_km2 = sum(area_m2)/1000000,
      seg_length_km = unique(length_m)/1000,
      seg_azrh_rad = unique(azrh))

  dist_dat_nat_geo <- dist_dat_nat %>%
    left_join(geo_united, by='seg_id_nat')

  readr::write_csv(dist_dat_nat_geo, path = out_file)
}

filter_obs <- function(dat_ind, subset, out_ind) {
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  subset_sites <- readRDS(subset)

  filt_dat <- filter(dat, seg_id_nat %in% unique(subset_sites$seg_id_nat))

  write.csv(filt_dat, as_data_file(out_ind), row.names = FALSE)
  gd_put(out_ind)
}

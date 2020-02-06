# pull data for penn state peeps
filter_sites <- function(geo_dat, dat_ind, years, obs_per_year, min_drainage, max_drainage, out_file) {
  # first find sites that are highly observed
  cross <- readRDS(sc_retrieve(dat_ind)) %>% 
    select(subseg_id, seg_id_nat) %>% distinct()
  
  # get site metadata from national layer
  geo <- sf::read_sf(geo_dat, layer='nsegmentNationalIdentifier') %>%
    filter(seg_id_nat %in% cross$seg_id_nat)
  
  dat <- readRDS(sc_retrieve(dat_ind)) %>% 
    group_by(subseg_id) %>%
    summarize(n_dates = n())
  
  dat_years <- readRDS(sc_retrieve(dat_ind)) %>%
    mutate(year = lubridate::year(date)) %>%
    filter(year >=1980) %>%
    group_by(subseg_id, year) %>%
    summarize(n_per_year = n())
  
  # how about -- 10 years with >150 days (5 months) of obs?
  dat_filtered <- dat_years %>%
    filter(n_per_year >= obs_per_year) %>%
    group_by(subseg_id) %>%
    summarize(n_years_5months = n()) %>%
    ungroup() %>%
    filter(n_years_5months >= years) %>%
    left_join(cross) %>%
    left_join(dat) %>%
    left_join(select(geo, MAX_CUMDRAINAG, seg_id_nat)) %>%
    arrange(MAX_CUMDRAINAG) %>%
    mutate(rank = 1:n())
  
  # 27 sites
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
  drb_net <- readRDS(sc_retrieve(network_ind))
  crosswalk_sf <- readRDS(sc_retrieve(cross_ind)) %>%
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
  
  dist_mat <- readRDS('1_network/out/distance_matrix.rds')
  dist_mat <- dist_mat$upstream
  network <- readRDS('1_network/out/network.rds')
  
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


subset_dist <- function(dat, dist_ind, network_ind, out_ind){
  
sites <- readRDS(dat)
dist_mat <- readRDS(sc_retrieve(dist_ind))
dist_mat <- dist_mat$updown
network <- readRDS(sc_retrieve(network_ind))

subset_dist <- dist_mat[sites$subseg_id, ]
subset_dist <- subset_dist[, sites$subseg_id]

saveRDS(subset_dist, as_data_file(out_ind))

gd_put(out_ind)

}

write_distance <- function(dat, out_ind) {
  subset_dist <- readRDS(dat)
  write.csv(subset_dist, as_data_file(out_ind), row.names = TRUE)
  gd_put(out_ind)
}

dist_heatmap2 <- function(dist_ind, labels=c('subseg_id','seg_id_nat'), title, out_file) {
  
  labels <- match.arg(labels)
  dat <- readRDS(sc_retrieve(dist_ind))
  
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

get_upstream_sites <- function(dist_ind, network_ind, sites, out_file) {
  dist <- readRDS(sc_retrieve(dist_ind))
  dist <- dist$upstream
  sites <- readRDS(sites)
  
  network <- readRDS(sc_retrieve(network_ind))
  network <- network$edges %>% dplyr::select(subseg_id, seg_id_nat) %>% sf::st_drop_geometry()
  #new_names <- network$seg_id_nat[network$subseg_id %in% row.names(dist)]
  
  dist_red <- dist[row.names(dist) %in% sites$subseg_id, ]
  
  dist_dat <- data.frame(from_reach = row.names(dist_red), stringsAsFactors = FALSE)
  dist_dat[, 2:(1+ncol(dist_red))] <- dist_red
  names(dist_dat)[2:ncol(dist_dat)] <- colnames(dist_red)
  
  dist_dat <- tidyr::gather(dist_dat, key = 'to_reach', value = 'distance', -from_reach) %>%
    filter(!is.infinite(distance) & distance > 0) %>% select(-distance) %>% arrange(from_reach)
  
  # now translate to national IDs

  dist_dat_nat <- dist_dat %>%
    left_join(network, by = c('from_reach' = 'subseg_id')) %>%
    select(-from_reach) %>% rename(from_reach = seg_id_nat) %>%
    left_join(network, by = c('to_reach' = 'subseg_id')) %>%
    select(-to_reach) %>% rename(to_reach = seg_id_nat) %>%
    filter(!is.na(to_reach)) %>%
    select(from_reach, to_reach)
  
  # add sites with no upstream reaches back in
  dist_dat_nat <- add_row(dist_dat_nat, from_reach = sites$seg_id_nat[!sites$seg_id_nat %in% unique(dist_dat_nat$from_reach)])
  
  write.csv(dist_dat_nat, file = out_file, row.names = FALSE)
  
}

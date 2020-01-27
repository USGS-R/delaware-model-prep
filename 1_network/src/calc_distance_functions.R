library(tidyverse)
library(sf)
library(igraph)

#' Calculate distance matrix for reaches in network
#'
#' Uses igraph to ocmpute a matrix of distances in meters.
#'
#' @param network the network from which to calculate distances (.rds)
#' @param labels
#' @return distance matrix
#' @examples

calc_dist_matrices <- function(network_ind, labels=c('subseg_id','seg_id_nat'), out_ind) {
  labels <- match.arg(labels)
  
  network <- readRDS(sc_retrieve(network_ind))
  # recombine the reach information so that what I was calling the "edges" of
  # the river network (the river reaches) become the vertices of the igraph
  # object, where the distance from one vertex to the next is the distance (or
  # travel time) along the second vertex=reach
  edges <- network$edges %>%
    st_drop_geometry() %>%
    select(subseg_id, to_subseg) %>%
    left_join(select(st_set_geometry(network$edges, NULL), subseg_id, subseg_length), by=c('to_subseg'='subseg_id')) %>%
    rename(from_reach=subseg_id, to_reach=to_subseg, reach_length=subseg_length) %>%
    # remove the 7 rows where a subseg_id empties to NA (the river outlets); the subseg_ids are still present in the network as to_subsegs
    filter(!is.na(to_reach)) 
  if(labels=='seg_id_nat') {
    subseg_seg_map <- network$edges %>% st_drop_geometry() %>% select(subseg_id, seg_id_nat)
    edges <- edges %>%
      mutate(
        from_reach = subseg_seg_map[match(from_reach, subseg_seg_map$subseg_id), ]$seg_id_nat,
        to_reach = subseg_seg_map[match(to_reach, subseg_seg_map$subseg_id), ]$seg_id_nat)
    if(!all(complete.cases(edges))) {
      stop('labels=seg_id_nat but not all reaches have a seg_id_nat assigned')
    }
  }
  graph <-  igraph::graph_from_data_frame(edges, directed = TRUE) # df must contain "a symbolic edge list in the first two columns. Additional columns are considered as edge attributes"
  
  dists_complete <- distances(graph, weights = edge.attributes(graph)$reach_length, mode='all') # symmetric
  dists_downstream <- distances(graph, weights = edge.attributes(graph)$reach_length, mode='out') # shortest paths FROM each vertex.
  dists_upstream <- distances(graph, weights = edge.attributes(graph)$reach_length, mode='in') # shortest paths TO each vertex (flowing upstream only)
  dists_updown <- dists_downstream
  for(i in 1:nrow(dists_downstream)) {
    for(j in 1:ncol(dists_downstream)) {
      if(is.infinite(dists_downstream[i,j]) & !is.infinite(dists_upstream[i,j])) {
        dists_updown[i,j] <- -dists_upstream[i,j]
      }
    }
  }
  out_file <- as_data_file(out_ind)
  saveRDS(list(complete=dists_complete, downstream=dists_downstream, upstream=dists_upstream, updown=dists_updown), out_file)
  gd_put(out_ind)
}


dist_heatmap <- function(dat, labels=c('subseg_id','seg_id_nat'), title, save_as) {
  labels <- match.arg(labels)
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
  if(!missing(save_as)) {
    ggsave(save_as, plot=g, width=11, height=10)
  }
  return(g)
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
  labels <- match.arg(labels)
  dists_from_start <- dist_mat[as.character(from_reach),]/1000
  pt_dist_reaches <- mutate(network$edges, reach_id = if(labels == 'subseg_id') subseg_id else seg_id_nat) %>%
    mutate(dist_from_start = dists_from_start[as.character(reach_id)]) %>%
    filter(is.finite(dist_from_start) | reach_id == from_reach)
  pt_dist_vertices <- network$vertices %>%
    right_join(pt_dist_reaches %>% st_drop_geometry(), by=c('point_ids'='end_pt')) %>%
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
  if(!missing(out_file)) {
    ggsave(out_file, plot=g, width=5, height=7)
  }
  return(g)
}

# save distance matrices in numpy format
save_dist_matrices <- function(dist_mat_ind, out_ind) {
  np <- reticulate::import('numpy')
  
  dist_mat_list <- readRDS(sc_retrieve(dist_mat_ind))
  
  out_file <- as_data_file(out_ind)
  np$savez_compressed(
    file=out_file,
    downstream=dist_mat_list$downstream,
    upstream=dist_mat_list$upstream,
    complete=dist_mat_list$complete,
    updown=dist_mat_list$updown)
  # R access examples:
  # loaded <- np$load('out/dists.npz')
  # loaded$files
  # loaded$f[['updown']]
  
  gd_put(out_ind)
  
  
}
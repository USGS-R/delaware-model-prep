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

  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))
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





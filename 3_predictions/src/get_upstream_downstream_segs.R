

create_graph = function(model_run_loc,
                        param_file = 'input/myparam.param',
                        model_fabric_file = 'GIS/Segments_subset.shp',
                        n_segments = 456){

  # use this to organize connect to seg_id_nat
  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  model_locations = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                           model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  # tosegment is a parameter in the param file
  params = readLines(file.path(model_run_loc, param_file))

  param_loc_start = grep('tosegment_nhm', params) + 5
  param_loc_end = param_loc_start + n_segments - 1

  to_seg_id_nat_vec = params[param_loc_start:param_loc_end]

  # make data frame of network to use in igraph package
  network_map = model_locations %>%
    mutate(to_seg_id_nat = to_seg_id_nat_vec) %>%
    left_join(select(model_locations, seg_id_nat), by = c('to_seg_id_nat' = 'seg_id_nat')) %>%
    rename(from_seg_id_nat = seg_id_nat) %>%
    mutate(to_seg_id_nat = ifelse(to_seg_id_nat == 0 , NA, to_seg_id_nat)) %>%
    select(from_seg_id_nat, to_seg_id_nat)

  network_graph = igraph::graph_from_data_frame(network_map, directed = TRUE)

  return(network_graph)
}


#' @param seg_id_nat segments of the network for which you want upstream segments
#'
get_upstream_segs = function(seg_id_nat,
                             model_run_loc,
                             param_file = 'input/myparam.param',
                             model_fabric_file = 'GIS/Segments_subset.shp',
                             n_segments = 456){

  seg_id_nat = as.character(seg_id_nat)

  network_graph = suppressWarnings(create_graph(param_file = param_file,
                                                model_run_loc = model_run_loc,
                                                model_fabric_file = model_fabric_file,
                                                n_segments = n_segments))

  # see https://github.com/robertness/lucy/blob/master/R/lucy.R for upstream /downstream functions
  # grab upstream segments of seg_id_nat supplied
  upstream <- igraph::shortest.paths(graph = network_graph,
                                     v = igraph::V(network_graph),
                                     to = seg_id_nat, mode = "out")

  # create named list of segments upstream
  out = sapply(colnames(upstream), function(seg){
    cur = names(upstream[!is.infinite(upstream[,seg]), seg])
  }, USE.NAMES = T)

  return(out)
}

#' @param seg_id_nat segments of the network for which you want upstream segments
#'
get_direct_upstream_segs = function(seg_id_nat,
                                    model_run_loc,
                                    param_file = 'input/myparam.param',
                                    model_fabric_file = 'GIS/Segments_subset.shp',
                                    n_segments = 456){

  seg_id_nat = as.character(seg_id_nat)

  network_graph = suppressWarnings(create_graph(param_file = param_file,
                                                model_run_loc = model_run_loc,
                                                model_fabric_file = model_fabric_file,
                                                n_segments = n_segments))

  # see https://github.com/robertness/lucy/blob/master/R/lucy.R for upstream /downstream functions
  # grab upstream segments of seg_id_nat supplied
  upstream <- igraph::shortest.paths(graph = network_graph,
                                     v = igraph::V(network_graph),
                                     to = seg_id_nat, mode = "out")

  # create named list of segments upstream
  out = sapply(colnames(upstream), function(seg){
    cur = names(which(upstream[,seg] == min(upstream[!is.infinite(upstream[,seg]) & upstream[,seg] !=0, seg])))
  }, USE.NAMES = T)

  return(out)
}

#' @param seg_id_nat segments of the network for which you want upstream segments
#'
get_direct_downstream_segs = function(seg_id_nat,
                                      model_run_loc,
                                      param_file = 'input/myparam.param',
                                      model_fabric_file = 'GIS/Segments_subset.shp',
                                      n_segments = 456){

  seg_id_nat = as.character(seg_id_nat)

  network_graph = suppressWarnings(create_graph(param_file = param_file,
                                                model_run_loc = model_run_loc,
                                                model_fabric_file = model_fabric_file,
                                                n_segments = n_segments))

  # see https://github.com/robertness/lucy/blob/master/R/lucy.R for upstream /downstream functions
  # grab upstream segments of seg_id_nat supplied
  upstream <- igraph::shortest.paths(graph = network_graph,
                                     v = igraph::V(network_graph),
                                     to = seg_id_nat, mode = "in")

  # create named list of segments upstream
  out = sapply(colnames(upstream), function(seg){
    cur = names(which(upstream[,seg] == min(upstream[!is.infinite(upstream[,seg]) & upstream[,seg] !=0, seg])))
  }, USE.NAMES = T)

  return(out)
}

#' @param seg_id_nat segments of the network for which you want upstream segments
#'
get_downstream_segs = function(seg_id_nat,
                               model_run_loc,
                               param_file = 'input/myparam.param',
                               model_fabric_file = 'GIS/Segments_subset.shp',
                               n_segments = 456){

  seg_id_nat = as.character(seg_id_nat)


  network_graph = suppressWarnings(create_graph(param_file = param_file,
                                                model_run_loc = model_run_loc,
                                                model_fabric_file = model_fabric_file,
                                                n_segments = n_segments))

  # see https://github.com/robertness/lucy/blob/master/R/lucy.R for upstream /downstream functions
  # grab upstream segments of seg_id_nat supplied
  upstream <- igraph::shortest.paths(graph = network_graph,
                                     v = igraph::V(network_graph),
                                     to = seg_id_nat, mode = "in")

  # create named list of segments upstream
  out = sapply(colnames(upstream), function(seg){
    cur = names(upstream[!is.infinite(upstream[,seg]), seg])
  }, USE.NAMES = T)

  return(out)
}

#' Test if a segment is Downstream or Upstream of Another
#'
is_b_downstream_of_a <- function(seg_id_nat_a,
                                 seg_id_nat_b,
                                 model_run_loc,
                                 param_file = 'input/myparam.param',
                                 model_fabric_file = 'GIS/Segments_subset.shp',
                                 n_segments = 456){

  network_graph = suppressWarnings(create_graph(param_file = param_file,
                                                model_run_loc = model_run_loc,
                                                model_fabric_file = model_fabric_file,
                                                n_segments = n_segments))

  out <- suppressWarnings(shortest.paths(graph = network_graph,
                                        v = seg_id_nat_a,
                                        to = seg_id_nat_b,
                                        mode = "out",
                                        algorithm = "unweighted"))[V(network_graph)[seg_id_nat_a]$name, V(network_graph)[seg_id_nat_b]$name]
  is.finite(out)
}

#' Test if a segment is Downstream or Upstream of Another
#'
is_b_upstream_of_a <- function(seg_id_nat_a,
                               seg_id_nat_b,
                               model_run_loc,
                               param_file = 'input/myparam.param',
                               model_fabric_file = 'GIS/Segments_subset.shp',
                               n_segments = 456){

  network_graph = suppressWarnings(create_graph(param_file = param_file,
                                                model_run_loc = model_run_loc,
                                                model_fabric_file = model_fabric_file,
                                                n_segments = n_segments))

  out <- suppressWarnings(shortest.paths(graph = network_graph,
                                         v = seg_id_nat_a,
                                         to = seg_id_nat_b,
                                         mode = "in",
                                         algorithm = "unweighted"))[V(network_graph)[seg_id_nat_a]$name, V(network_graph)[seg_id_nat_b]$name]
  is.finite(out)
}



create_subbasin = function(subbasin_seg_id_nat,
                           model_run_loc,
                           model_fabric_file = 'GIS/Segments_subset.shp'){

  subbasin_model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file)) %>%
    dplyr::filter(seg_id_nat %in% subbasin_seg_id_nat)

  return(subbasin_model_fabric)
}






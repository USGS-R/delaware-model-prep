library(sf)
library(dplyr)

#' @param dams_file directory of dams shapefile
#' @param reservoirs_file directory of reservoir shapefile
#' @param boundary polygon of basin boundaries to subset dams/reservoirs by
#' @param outind output indicator file
filter_dams_reservoirs_by_boundary <- function(dams_shp_ind, reservoirs_shp_ind, 
                                               boundary, out_ind) {
  #dams are points, reservoirs are polygons
  dams_shp <- read_sf(as_data_file(dams_shp_ind)) %>% 
    st_transform(crs = 102039) %>% 
    filter(COUNTRY == "United States")
  
  #Assumes GRAND IDs match up for corresponding reservoirs and dams
  #There are some empty/invalid geometries outside the US, this 
  #avoids them
  res_shp <- read_sf(as_data_file(reservoirs_shp_ind)) %>% 
    st_transform(crs = 102039) %>% 
    filter(GRAND_ID %in% dams_shp$GRAND_ID)
  
  boundary <- readRDS('1_network/out/boundary.rds')
  
  subset_dams <- st_intersection(dams_shp, boundary)
  subset_res <- st_intersection(res_shp, boundary)
  
  output <- list(dams = subset_dams, reservoirs = subset_res)
  saveRDS(output, file = as_data_file(out_ind))
  sc_indicate(ind_file = out_ind)
}

intersect_network_with_reservoirs <- function(stream_network_file, dams_reservoirs_file, out_ind) {
  stream_network <- readRDS(stream_network_file)
  dams_reservoirs <- readRDS(dams_reservoirs_file)
  
  network_edges_tibble <- as_tibble(stream_network$edges)
  network_vertices_tibble <- as_tibble(stream_network$vertices)
  network_vertices_tibble_separated <- network_vertices_tibble %>% 
    tidyr::separate_rows(ends_subseg, sep = ";")
  dams_tibble <- as_tibble(dams_reservoirs$dams)
  reservoirs_tibble <- as_tibble(dams_reservoirs$reservoirs)
  
  network_res_intersection <- st_intersection(stream_network$edges, subset_res) %>% 
    mutate(intersected_subseg_length = st_length(geometry),
           frac_res_overlap = intersected_subseg_length/subseg_length) %>% 
    rename(intersected_geometry = geometry) %>% 
    left_join(reservoirs_tibble) %>% 
    rename(reservoir_geometry = geometry) %>% 
    left_join(dams_tibble) %>% 
    rename(dam_geometry = geometry) %>% 
    #join on inlet and outlet vertices
    left_join(network_vertices_tibble, by = c(subseg_id = "starts_subseg")) %>% 
    select(everything(), inlet_point_geom = geometry, -point_ids, -ends_subseg, -XY) %>% 
    left_join(network_vertices_tibble_separated, by = c(subseg_id = "ends_subseg")) %>% 
    select(everything(), outlet_point_geom = geometry, -point_ids, -starts_subseg, -XY) %>% 
    mutate(reservoir_centroid = st_centroid(reservoir_geometry)) %>% 
    as_tibble() %>% #drop sf class to allow joining on ids rather than a spatial operation (st_join)
    right_join(network_edges_tibble,
               by = c("subseg_id", "subseg_seg", "subseg_updown", "subseg_length", 
                      "start_pt", "end_pt", "from_segs", "to_seg", "to_subseg", 
                      "seg_id_nat"))  %>% 
    #compute distances to dam and reservoir centroid, from each end and reach centroid
    mutate(subseg_centroid = st_centroid(geometry)) %>% 
    rowwise() %>% 
    mutate(dist_inlet_to_dam = st_distance(inlet_point_geom, dam_geometry),
           dist_inlet_to_res_centroid = st_distance(inlet_point_geom, reservoir_centroid),
           dist_outlet_to_dam = st_distance(outlet_point_geom, dam_geometry),
           dist_outlet_to_res_centroid = st_distance(outlet_point_geom, reservoir_centroid), 
           dist_reach_centroid_to_dam = st_distance(subseg_centroid, dam_geometry),
           dist_reach_centroid_to_res_centroid = st_distance(subseg_centroid, reservoir_centroid),
           outlet_intersects = as.numeric(st_intersects(outlet_point_geom, reservoir_geometry)),
           inlet_intersects = as.numeric(st_intersects(inlet_point_geom, reservoir_geometry)))
  
  saveRDS(network_res_intersection, file = as_data_file(out_ind))
  sc_indicate(out_ind)
}

##### plot function ######
plot_reservoir_reach_overlaps <- function() {
  #generate plots to check
  subset_res <- rename(subset_res, reservoir_geometry = geometry)
  subset_dams <- rename(subset_dams, dam_geometry = geometry)
  network_res_intersection_all_geoms <- left_join(network_res_intersection,
                                                  subset_res, 
                                                  by = "GRAND_ID") %>% 
    left_join(subset_dams, by = "GRAND_ID")
  library(ggplot2)
  for(reservoir in na.omit(unique(network_res_intersection_all_geoms$GRAND_ID))) {
    reservoir_df <- network_res_intersection_all_geoms %>% 
      filter(GRAND_ID == reservoir) %>% 
      mutate()
    ggplot(reservoir_df, aes(geometry = geometry)) + 
      geom_sf(data = reservoir_df[1,], aes(geometry = reservoir_geometry, fill = NULL)) +
      geom_sf(aes(color = subseg_id)) +
      theme(legend.position = "none") + #legend is causing an error.  Unsure why
      geom_sf(aes(geometry = dam_geometry), shape = 15, size = 4) + 
      coord_sf(crs = 102039) + 
      ggtitle(label = paste("GRAND_ID:", reservoir, reservoir_df$RES_NAME[1]),
              subtitle = paste("Seg IDs:", paste(reservoir_df$subseg_id, collapse = ",")))
    ggsave(filename = paste0(reservoir, ".png"))
  }
}
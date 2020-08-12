#' @param zip char path to input zip file containing shapefile
#' @param extract_dir char directory to unzip to, and read shapefile from
#' @param out_ind char indicator file to represent output
zipped_shp_to_rds <- function(zip_ind, out_ind) {

  zip <- scipiper::as_data_file(zip_ind)
  extract_dir <- file.path(tempdir(),
                           tools::file_path_sans_ext(basename(zip)))
  unzip(sc_retrieve(zip_ind, 'getters.yml'), exdir = extract_dir)
  read_sf(extract_dir) %>% saveRDS(file = as_data_file(out_ind))
  unlink(extract_dir, recursive = TRUE)
  gd_put(out_ind)
}

#' @param dams_file directory of dams shapefile
#' @param reservoirs_file directory of reservoir shapefile
#' @param boundary polygon of basin boundaries to subset dams/reservoirs by
#' @param outind output indicator file
filter_dams_reservoirs_by_boundary <- function(dams_shp_ind, reservoirs_shp_ind,
                                               boundary_ind, out_ind) {

  boundary <- readRDS(sc_retrieve(boundary_ind, 'getters.yml'))

  #dams are points, reservoirs are polygons
  dams_shp <- readRDS(sc_retrieve(dams_shp_ind, 'getters.yml')) %>%
    st_transform(crs = sf::st_crs(boundary)) %>%
    filter(COUNTRY == "United States")

  #Assumes GRAND IDs match up for corresponding reservoirs and dams
  #There are some empty/invalid geometries outside the US, this
  #avoids them
  res_shp <- readRDS(sc_retrieve(reservoirs_shp_ind, 'getters.yml')) %>%
    st_transform(crs = sf::st_crs(boundary)) %>%
    filter(GRAND_ID %in% dams_shp$GRAND_ID)

  subset_dams <- st_intersection(dams_shp, boundary)
  subset_res <- st_intersection(st_make_valid(res_shp), boundary)
  output <- list(dams = subset_dams, reservoirs = subset_res)
  saveRDS(output, file = as_data_file(out_ind))
  gd_put(out_ind)
}


#' Create a data frame for each subsegment with assorted into about reservoirs they intersect
#' @param stream_network_file RDS file containing stream network edge and vertex geometries
#' @param dams_reservoirs_file RDS file containing sf objects for dams and reservoirs in the basin (in a list
#' @param out_ind char output indicator file
intersect_network_with_reservoirs <- function(stream_network_ind, dams_reservoirs_ind, out_ind) {

  stream_network <- readRDS(sc_retrieve(stream_network_ind, 'getters.yml'))
  dams_reservoirs <- readRDS(sc_retrieve(dams_reservoirs_ind, 'getters.yml'))

  #create some non-sf tibbles to allow joining by ID rather than geometry
  network_edges_tibble <- as_tibble(stream_network$edges)
  network_vertices_tibble <- as_tibble(stream_network$vertices)
  network_vertices_tibble_separated <- network_vertices_tibble %>%
    tidyr::separate_rows(ends_subseg, sep = ";")
  dams_tibble <- as_tibble(dams_reservoirs$dams) %>%
    rename(dam_geometry = geometry)
  reservoirs_tibble <- as_tibble(dams_reservoirs$reservoirs) %>%
    rename(reservoir_geometry = geometry)

  network_res_intersection <- st_intersection(stream_network$edges, dams_reservoirs$reservoirs) %>%
    mutate(intersected_subseg_length = st_length(geometry),
           frac_res_overlap = intersected_subseg_length/subseg_length) %>%
    rename(intersected_geometry = geometry) %>%
    left_join(reservoirs_tibble) %>%
    left_join(dams_tibble) %>%
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
  gd_put(out_ind)
}

##### plot function ######
#not built into pipeline
#' Plot each reservoir, with intersecting subsegments and dam
#' @param subseg_reservoir_rds rds 1_network/out/subseg_reservoir_mapping.rds.ind from pipeline
plot_reservoir_reach_overlaps <- function(subseg_reservoir_rds = '1_network/out/subseg_reservoir_mapping.rds',
                                          output_dir = '1_network/tmp') {
  network_res_intersection_all_geoms <- readRDS(subseg_reservoir_rds)
  for(reservoir in na.omit(unique(network_res_intersection_all_geoms$GRAND_ID))) {
    reservoir_df <- network_res_intersection_all_geoms %>%
      filter(GRAND_ID == reservoir) %>%
      mutate()
    ggplot(reservoir_df, aes(geometry = geometry)) +
      geom_sf(data = reservoir_df[1,], aes(geometry = reservoir_geometry)) +
      geom_sf(aes(color = subseg_id)) +
      theme(legend.position = "none") + #legend is causing an error.  Unsure why
      geom_sf(aes(geometry = dam_geometry), shape = 15, size = 4) +
      coord_sf(crs = 102039) +
      ggtitle(label = paste("GRAND_ID:", reservoir, reservoir_df$RES_NAME[1]),
              subtitle = paste("Seg IDs:", paste(reservoir_df$subseg_id, collapse = ",")))
    ggsave(filename = file.path(output_dir, paste0(reservoir, ".png")))
  }
}

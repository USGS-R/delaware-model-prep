library(sf)
library(dplyr)
#dams are points, reservoirs are polygons
dams_shp <- read_sf('~/Downloads/dams-rev01-global-shp/') %>% 
  st_transform(crs = 102039) %>% 
  filter(COUNTRY == "United States")

#Assumes GRAND IDs match up for corresponding reservoirs and dams
#There are some empty/invalid geometries outside the US, this 
#avoids them
res_shp <- read_sf('~/Downloads/reservoirs-rev01-global-shp') %>% 
  st_transform(crs = 102039) %>% 
  filter(GRAND_ID %in% dams_shp$GRAND_ID)

stream_network <- readRDS('1_network/out/network.rds')

boundary <- readRDS('1_network/out/boundary.rds')
boundary_bbox <- st_bbox(boundary)
dist_mat <- readRDS('1_network/out/subseg_distance_matrix.rds')

subset_dams <- st_intersection(dams_shp, boundary)
subset_res <- st_intersection(res_shp, boundary)

########### new function #####
network_edges_tibble <- as_tibble(stream_network$edges)
network_res_intersection <- st_intersection(stream_network$edges, subset_res) %>% 
  mutate(intersected_subseg_length = st_length(geometry),
         frac_res_overlap = intersected_subseg_length/subseg_length) %>% 
  rename(intersected_geometry = geometry) %>% 
  as_tibble() %>% #drop sf class to allow joining on ids rather than a spatial operation (st_join)
  right_join(network_edges_tibble,
             by = c("subseg_id", "subseg_seg", "subseg_updown", "subseg_length", 
                    "start_pt", "end_pt", "from_segs", "to_seg", "to_subseg", 
                    "seg_id_nat")) 

##### plot function ######
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

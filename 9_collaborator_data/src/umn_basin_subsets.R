library(tidyverse)
library(sf)
source('src/calc_dist_matrix.R')

# Read in network from refine_prms_network.R
reach_net <- readRDS('out/network_full.rds')
# Read in crosswalk to get site names and observation counts (used for subnetworks)
crosswalk_site_reach <- readRDS('out/crosswalk_site_reach.rds')
filtered_crosswalk <- crosswalk_site_reach %>%
  filter(bird_dist_to_subseg_m < 250, abs(fish_dist_to_outlet_m) < 5000) %>%
  mutate(n_obs_bin = base::cut(n_obs, breaks=c(min(n_obs), 10, 100, 1000, max(n_obs)), right=FALSE, labels=c('1-10', '10-100', '100-1000', '1000+')))

#### Full network ####

# Create, save, and explore full-network distance matrices and maps
dists <- calc_dist_matrices(reach_net)
save_dist_matrices(dists, 'out/dists_full.npz')

dist_heatmap(dists$downstream, 'seg_id_nat', 'Downstream', 'out/dists_full_downstream.png')
dist_heatmap(dists$upstream, 'seg_id_nat', 'Upstream', 'out/dists_full_upstream.png')
dist_heatmap(dists$complete, 'seg_id_nat', 'Complete', 'out/dists_full_complete.png')
dist_heatmap(dists$updown, 'seg_id_nat', 'Updown', 'out/dists_full_updown.png')

# matrix subset tables: first 6 points
round(dists$downstream[1:6,1:6])
round(dists$upstream[1:6,1:6])
round(dists$complete[1:6,1:6])
round(dists$updown[1:6,1:6])

# single-reach example: from_pt=3d;4u, subseg_id=3_2 flows to to_pt=4d;5u;9d, to_subseg=4_1
filter(reach_net$edges, subseg_id=='4_1') # reach length = 1914 m
dists$downstream['3_2','4_1'] # 1914
dists$downstream['4_1','3_2'] # Inf
dists$upstream['3_2','4_1'] # Inf
dists$upstream['4_1','3_2'] # 1914
dists$updown['3_2','4_1'] == -dists$updown['4_1','3_2']

rch <- '884_1'
plot_dists(rch, dists$downstream, 'subseg_id', reach_net, 'Downstream', 'out/map_dists_full_downstream.png')
plot_dists(rch, dists$upstream, 'subseg_id', reach_net, 'Upstream', 'out/map_dists_full_upstream.png')
plot_dists(rch, dists$complete, 'subseg_id', reach_net, 'Complete Network', 'out/map_dists_full_complete.png')
plot_dists(rch, dists$updown, 'subseg_id', reach_net, 'Upstream or Downstream', 'out/map_dists_full_updown.png')

#### ~100-edge subnetwork ####

make_subnetwork <- function(lower_reach, exclude_reaches, drb_net, dists, labels=c('subseg_id','seg_id_nat')) {
  labels <- match.arg(labels)
  
  up_from_lowermost <- names(which(dists$upstream[lower_reach,] < Inf))
  if(length(exclude_reaches) > 0) {
    up_from_uppermost <- unlist(lapply(exclude_reaches, function(exclude_point) {
      names(which(dists$upstream[exclude_point,] < Inf))
    }))
    subnet_ids <- setdiff(up_from_lowermost, up_from_uppermost)
  } else {
    subnet_ids <- up_from_lowermost
  }
  if(labels == 'subseg_id') {
    subnet_reaches <- drb_net$edges %>% filter(subseg_id %in% subnet_ids)
  } else {
    subnet_reaches <- drb_net$edges %>% filter(seg_id_nat %in% subnet_ids)
  }
  subnet_reaches <- subnet_reaches %>% mutate(to_subseg = ifelse(subseg_id==lower_reach, NA, to_subseg))
  subnet_points <- filter(drb_net$vertices, point_ids %in% c(subnet_reaches$end_pt, subnet_reaches$start_pt))
  
  return(list(edges=subnet_reaches, vertices=subnet_points, lower_reach=lower_reach, exclude_reaches=exclude_reaches))
}
explore_subnetwork <- function(subnet, crosswalk, drb_net) {
  g <- ggplot(drb_net$edges) + geom_sf(color='lightgray') +
    geom_sf(data=subnet$edges, color='seagreen') +
    geom_sf(data=crosswalk, aes(color=n_obs_bin), size=1) +
    geom_sf(data=filter(drb_net$vertices, point_ids %in% subnet$exclude_reaches), shape=4, color='red') +
    scale_color_brewer('Number of Observations', palette=3) +
    theme_bw() +
    ggtitle('Filtered by bird and fish distance; showing observation counts')
  print(g)
  
  message(sprintf('%d edges, %d vertices', nrow(subnet$vertices), nrow(subnet$edges)))
  
  obs_count <- subnet$edges %>%
    st_drop_geometry() %>%
    left_join(st_drop_geometry(crosswalk), by='subseg_id') %>%
    group_by(subseg_id) %>%
    summarize(n_sites=length(which(!is.na(site_id))), n_obs=sum(n_obs))
  message(sprintf('%d observed reaches, %d observations total', length(which(obs_count$n_sites > 0)), sum(obs_count$n_obs, na.rm=TRUE)))
}
#subnet1 <- make_subnetwork(exclude_points=c("332u;64d;65d", "330d;341u"), lower_point='2752d;2757u', reach_net, dists)
subnet1 <- make_subnetwork(exclude_reaches=c('332_1','341_1'), lower_reach='2752_1', reach_net, dists, 'subseg_id')
explore_subnetwork(subnet1, filtered_crosswalk, reach_net)
# 173 edges, 174 vertices
# 130 observed reaches, 111503 observations total

subnet2 <- make_subnetwork(lower_reach='886_1', exclude_reaches=c(), reach_net, dists)
explore_subnetwork(subnet2, filtered_crosswalk, reach_net)
# 56 edges, 57 vertices
# 40 observed reaches, 14528 observations total

subnet3 <- make_subnetwork(lower_reach='288_1', exclude_reaches=c(), reach_net, dists)
explore_subnetwork(subnet3, filtered_crosswalk, reach_net)
# 29 edges, 30 vertices
# 12 observed reaches, 37077 observations total

subnet4 <- make_subnetwork(lower_reach='2748_1', exclude_reaches=c(), reach_net, dists) # 41 edges, 42 vertices
explore_subnetwork(subnet4, filtered_crosswalk, reach_net)
# 42 edges, 43 vertices
# 32 observed reaches, 74733 observations total

#### Save selected subnet ####

selected_subnet <- subnet4
saveRDS(selected_subnet, 'out/network_subset.rds')

dists_subnet <- calc_dist_matrices(selected_subnet, 'seg_id_nat')
save_dist_matrices(dists_subnet, 'out/dists_subset.npz')

plot_dists(2018, dists_subnet$downstream, 'seg_id_nat', selected_subnet, 'Subnetwork - Downstream', 'out/map_dists_subset_downstream.png')
plot_dists(2018, dists_subnet$upstream, 'seg_id_nat', selected_subnet, 'Subnetwork - Upstream', 'out/map_dists_subset_upstream.png')
dist_heatmap(dists_subnet$downstream, 'seg_id_nat', 'Downstream', 'out/dists_subset_downstream.png')
dist_heatmap(dists_subnet$upstream, 'seg_id_nat', 'Upstream', 'out/dists_subset_upstream.png')
dist_heatmap(dists_subnet$updown, 'seg_id_nat', 'Upstream and Downstream', 'out/dists_subset_updown.png')

plot_subnet <- function(subnet, reach_net, crosswalk, out_file) {
  just_beyonds <- reach_net$edges %>% 
    filter(start_pt %in% subnet$vertices$point_ids | end_pt %in% subnet$vertices) %>%
    filter(!subseg_id %in% subnet$edges$subseg_id)
  g <- ggplot(subnet$edges) + geom_sf(color='gold') +
    geom_sf(data=just_beyonds, color='gray') +
    geom_sf(data=subnet$vertices, color='gold', shape=4, size=2) +
    geom_sf(data=filter(crosswalk, subseg_id %in% subnet$edges$subseg_id), aes(color=n_obs_bin), size=2) +
    scale_color_brewer('Number of Observations', palette=3) +
    theme_bw() +
    ggtitle('Filtered by bird and fish distance; showing observation counts')
  ggsave(out_file, g, width=7, height=6)
  return(g)
}
plot_subnet(selected_subnet, reach_net, filtered_crosswalk, 'out/map_sites_subset.png')

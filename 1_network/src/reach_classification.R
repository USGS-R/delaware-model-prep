classify_segments <- function(out_ind, network_ind, reservoir_ind) {
  # read in data
  res <- readRDS(sc_retrieve(reservoir_ind, 'getters.yml'))
  network <- readRDS(sc_retrieve(network_ind, 'getters.yml'))

  # group by reservoir
  each_res <- ungroup(res) %>%
    filter(!is.na(GRAND_ID))

  # classify reaches by in reservor
  # for each reservoir:
  # find unique IDs
  test <- filter(each_res, GRAND_ID %in% unique(each_res$GRAND_ID)[4])
  touching_reservoir <- unique(test$subseg_seg)

  # first find segments that are touching reservoir
  # and classify in relation to dam/reservoir
  segs <- each_res %>%
    mutate(frac_overlap = as.numeric(frac_res_overlap)) %>%
    mutate(type = case_when(
      frac_overlap == 1 ~ 'within_reservoir',
      is.na(inlet_intersects) & is.na(outlet_intersects) ~ 'contains_reservoir',
      frac_overlap < 1 & !is.na(inlet_intersects) ~ 'reservoir_outlet_reach',
      frac_overlap < 1 & !is.na(outlet_intersects) ~ 'reservoir_inlet_reach'
    )) %>%
    mutate(dist_down_reservoir = case_when(
      type %in% c('within_reservoir', 'reservoir_inlet_reach') ~ units::as_units(0, 'm'),
      type %in% c('reservoir_outlet_reach', 'contains_reservoir') ~ dist_outlet_to_dam
    )) %>%
    group_by(GRAND_ID) %>%
    mutate(number_reaches = n()) %>% ungroup()

  # collapse segs down to a single row per segment
  # some segments have two reservoirs
  # in this case, take the min distance downstream
  segs_single <- segs %>%
    group_by(subseg_seg, seg_id_nat, to_seg, from_segs) %>%
    summarize(number_reservoirs = n(),
              GRAND_ID = paste(GRAND_ID, collapse = '; '),
              frac_overlap = sum(frac_overlap),
              type = ifelse(grepl('contains', paste(unique(type), collapse = '')), 'contains_reservoir', unique(type)),
              dist_down_reservoir = min(dist_down_reservoir)) %>%
    ungroup()

  res <- ungroup(res)

  # grab downstream reaches
  # keep track of which reservoirs the downstream segments are associated with

  downstream <- filter(segs_single, type %in% c('contains_reservoir', 'reservoir_outlet_reach')) %>%
    filter(!to_seg %in% unique(segs_single$subseg_seg))

  # calculate combined distances

  downstream1 <- downstream %>% pull(to_seg)

  # downstream1 distance
  # dist <- select(downstream, seg1 = subseg_seg, seg2 = to_seg, dist_down_reservoir, GRAND_ID) %>%
  #   left_join(st_drop_geometry(select(network$edges, seg2 = subseg_seg, subseg_length))) %>%
  #   rowwise() %>%
  #   mutate(dist_down_reservoir2 = sum(dist_down_reservoir, subseg_length)) %>%
  #   select(-subseg_length)

  downstream2 <- filter(res, subseg_seg %in% downstream1) %>%
    filter(!to_seg %in% segs_single$subseg_seg) %>%
    pull(to_seg)

  # dist2
  # dist_2 <- filter(res, subseg_seg %in% downstream1) %>%
  #   filter(!to_seg %in% segs_single$subseg_seg) %>%
  #   select(seg2 = subseg_seg, seg3 = to_seg) %>%
  #   left_join(select(res, seg3 = subseg_seg, subseg_length)) %>%
  #   right_join(dist) %>%
  #   rowwise() %>%
  #   mutate(dist_down_reservoir3 = sum(dist_down_reservoir2, subseg_length)) %>%
  #   select(-subseg_length)

  downstream12_reservoirs <- downstream %>% pull(GRAND_ID)

  down1_cross <- data.frame(subseg_seg = as.character(downstream1), closest_GRAND_ID = downstream12_reservoirs) %>%
    filter(!is.na(subseg_seg))
  down2_cross <- data.frame(subseg_seg = as.character(downstream2), closest_GRAND_ID = downstream12_reservoirs) %>%
    filter(!is.na(subseg_seg))

  # grab upstream reaches
  upstream1 <- segs_single$from_segs[segs_single$type %in% c('contains_reservoir', 'reservoir_inlet_reach')]

  upstream1_reservoirs <- segs_single$GRAND_ID[segs_single$type %in% c('contains_reservoir', 'reservoir_inlet_reach')]

  up1_cross <- data.frame(closest_GRAND_ID = rep(upstream1_reservoirs, grepl(';', upstream1) + 1),
                          subseg_seg = unlist(stringr::str_split(upstream1, ';'))) %>%
    filter(!subseg_seg %in% '') %>%
    filter(!subseg_seg %in% segs_single$subseg_seg) %>%
    distinct()
  # seg 262 is upstream of two different reservoirs. Which should it be associated with?

  upstream2 <- res$from_segs[res$subseg_seg %in% up1_cross$subseg_seg]
  up2_cross <- data.frame(closest_GRAND_ID = rep(up1_cross$closest_GRAND_ID, grepl(';', upstream2) + 1),
                          subseg_seg = unlist(stringr::str_split(upstream2, ';'))) %>%
    filter(!subseg_seg %in% '') %>%
    filter(!subseg_seg %in% segs_single$subseg_seg) %>%
    distinct()

  res_cross <- bind_rows(down1_cross, down2_cross, up1_cross, up2_cross)

  # create distance upstream metric
  # from_to <- select(segs_single, subseg_seg, from_segs, dist_down_reservoir, GRAND_ID) %>%
  #   tidyr::separate_rows('from_segs', sep = ';') %>%
  #   rename(seg_up1 = from_segs) %>%
  #   filter(!seg_up1 %in% segs_single$subseg_seg)

  # now use above info to classify reaches
  # data frame
  reach_class = select(network$edges, seg_id_nat, subseg_seg) %>%
    left_join(distinct(select(segs_single, seg_id_nat, type, GRAND_ID, frac_overlap))) %>%
    mutate(type_res = case_when(
      !is.na(type) ~ type,
      is.na(type) & subseg_seg %in% down1_cross$subseg_seg ~ 'downstream of reservoir (1)',
      is.na(type) & subseg_seg %in% down2_cross$subseg_seg ~ 'downstream of reservoir (2)',
      is.na(type) & subseg_seg %in% up1_cross$subseg_seg ~ 'upstream of reservoir (1)',
      is.na(type) & subseg_seg %in% up2_cross$subseg_seg ~ 'upstream of reservoir (2)',
      TRUE ~ 'reach'),
      subseg_seg = as.character(subseg_seg)) %>%
    left_join(res_cross) %>%
    mutate(nearest_GRAND_ID = ifelse(is.na(GRAND_ID), closest_GRAND_ID, GRAND_ID)) %>%
    select(-closest_GRAND_ID, -GRAND_ID, -type) %>%
    filter(!is.na(seg_id_nat)) %>%
    st_drop_geometry()

  saveRDS(reach_class, as_data_file(out_ind))
  gd_put(out_ind)

}




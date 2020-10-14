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

plotting_functions <- function(){
  library(RColorBrewer)
  reach_class$type_res <- factor(reach_class$type_res, levels = levels(factor(reach_class$type_res))[c(8,7,5,1,9,6,2,3,4)])

  p <- ggplot() +
    geom_sf(data = res$reservoir_geometry, fill = 'lightblue3', color = 'lightblue3', size = 2.2) +
    geom_sf(data = reach_class, aes(color = type_res), size = 0.6) +
    scale_color_manual(values = c(RColorBrewer::brewer.pal(8, 'RdYlBu'), 'lightgray')) +
    theme_bw() +
    theme(axis.text = element_blank(), panel.grid = element_blank()) +
    labs(color = 'Reach type')

  ggsave('reservoir_reach_map.png', p, height = 6, width = 6)

  #
  ggplot(lord, aes(x = date, y = temp_c)) +
    geom_point() +
    geom_line(data = lord, aes(x = date, y = sntemp_temp_c), color = 'purple') +
    geom_line(data = lord, aes(x = date, y = rgcn2_full_temp_c), color = 'dodgerblue')


  # for each reservoir, plot up/down data
  head(dat)
  unique(dat$site_id)

  reservoirs <- unique(reach_class$nearest_GRAND_ID)
  reservoirs <- reservoirs[!is.na(reservoirs)]
  for (i in 1:length(reservoirs)) {
    temp_reservoirs <- unlist(stringr::str_split(reservoirs[i], pattern = '; '))
    reaches <- filter(reach_class, nearest_GRAND_ID %in% reservoirs[i]) %>%
      filter(!is.na(seg_id_nat)) %>% mutate(seg_id_nat = as.character(seg_id_nat))

    temp_dat <- filter(dat, seg_id_nat %in% reaches$seg_id_nat) %>%
      left_join(select(reaches, seg_id_nat, type_res)) %>%
      filter(date > '2004-01-01' & date < '2011-01-01') %>%
      filter(!is.na(temp_c))

    mod_dat <- filter(dat, seg_id_nat %in% reaches$seg_id_nat) %>%
      left_join(select(reaches, seg_id_nat, type_res)) %>%
      filter(lubridate::year(date) %in% 2018:2020) %>%
      filter(!is.na(temp_c) & model %in% 'temp_c')

    train_count <- filter(dat, seg_id_nat %in% reaches$seg_id_nat) %>%
      left_join(select(reaches, seg_id_nat, type_res)) %>%
      filter(date < '2004-10-01') %>%
      filter(model %in% 'temp_c' & !is.na(temp_c)) %>%
      group_by(seg_id_nat, type_res) %>%
      summarize(n = n())

    # reservoir name
    res_name <- unique(res$RES_NAME[res$GRAND_ID %in% temp_reservoirs])
    if(any(is.na(res_name))){
      res_name <- unique(res$DAM_NAME[res$GRAND_ID %in% temp_reservoirs])
      if (length(res_name) >1 ){res_name = paste(res_name, collapse = ' & ')}
    }

    if (nrow(mod_dat) > 0) {
      mod_plot <- ggplot(mod_dat, aes(x = date, y = temp_c)) +
        geom_point(alpha = 0.7, size = 0.5, aes(color = type_res)) +
        geom_line(aes(color = type_res), alpha = 0.3) +
        theme_bw() +
        labs(x = '', y = 'Water temp [deg C]', title = res_name, color = 'Stream type')

      ggsave(file.path('out', paste0('modern_obs_dat', reservoirs[i], '.png')), mod_plot, height = 3, width = 6)

    }

    bb <- st_as_sfc(st_bbox(reaches))
    bbox_exp <- st_as_sfc(st_bbox(c(bb[1]*.99, bb[2]*.99, bb[3]*1.01, bb[4]*1.01)))
    p_map <- ggplot() +
      geom_sf(data = res$reservoir_geometry, color = 'lightblue', size = 2) +
      geom_sf(data = res$reservoir_geometry[res$GRAND_ID %in% temp_reservoirs], color = 'lightblue4', size = 2) +
      geom_sf(data = network$edges, color = 'darkgray') +
      geom_sf(data = reaches, aes(color = type_res)) +
      geom_sf(data = bb, fill = NA, color = 'red', size = 0.5) +
      theme_bw() +
      guides(color = 'none') +
      labs() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank()) +
      coord_sf(xlim = st_bbox(network$edges)[c(1,3)],
               ylim = st_bbox(network$edges)[c(2,4)])

    p_map2 <- ggplot() +
      geom_sf(data = res$reservoir_geometry, color = 'lightblue', size = 1.5) +
      #geom_sf(data = res$reservoir_geometry[res$GRAND_ID %in% temp_reservoirs], color = 'lightblue4', size = 2) +
      geom_sf(data = network$edges, color = 'darkgray', size = 0.1) +
      #geom_sf(data = reaches, aes(color = type_res)) +
      geom_sf(data = bb, fill = NA, color = 'red', size = 0.5) +
      theme_bw() +
      #guides(color = 'none') +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank()) +
      coord_sf(xlim = st_bbox(network$edges)[c(1,3)],
               ylim = st_bbox(network$edges)[c(2,4)])

    p_map2 <- ggplot() +
      geom_sf(data = res$reservoir_geometry[res$GRAND_ID %in% temp_reservoirs], color = 'lightblue4', fill = 'lightblue4',size = 2) +
      geom_sf(data = reaches, aes(color = type_res)) +
      theme_bw() +
      labs(title = res_name) +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_rect(color = 'red'),
            legend.position = 'bottom', legend.direction = 'vertical',
            legend.text = element_text(size = 8),
            legend.title = element_blank(), legend.background = element_blank())

    p_map2_nolegend <- p_map2 + guides(color = 'none')
    p_map2_nolegend_v2 <- p_map2_nolegend + theme(panel.border = element_blank())

    map_legend <- cowplot::get_legend(p_map2)
    library(cowplot)
    p_map_out <- ggdraw() +
      draw_plot(p_map, x = -0.2, y = 0, height = 0.9) +
      draw_plot(p_map2_nolegend, x = .2, y = 0.25, height = 0.6) +
      draw_plot(map_legend, x = 0.21, y = 0.08, height = 0.2)

    ggdraw() +
      draw_plot(p_map2_nolegend_v2, x = -0.2) +
      draw_plot(p_map2, y = 0.4, x = 0.2, height = 0.55) +
      draw_plot(map_legend, x = 0.25, y = 0.2, height = 0.2)


    ggsave(file.path('out', paste0('map', reservoirs[i], '.png')), p_map_out, height = 6, width = 6)

    highest_year <- temp_dat %>%
      filter(model %in% 'temp_c') %>%
      group_by(year = lubridate::year(date)) %>%
      summarize(n = n()) %>%
      slice(which.max(n))

    p1 <- ggplot(filter(temp_dat, model%in% 'temp_c'), aes(x = date, y = temp_c)) +
      geom_point(alpha = 0.7, size = 0.5) +
      geom_line(data = filter(temp_dat, !model%in% 'temp_c'),
                aes(x = date, y = temp_c, color = model), alpha = 0.7) +
      facet_wrap(~type_res, ncol = 1) +
      theme_bw() +
      labs(x = '', y = 'Water temp [deg C]', title = res_name)

    ggsave(file.path('out', paste0('pred_observed_test_', reservoirs[i], '.png')), p1, height = 8, width = 8)

    temp_dat <- filter(temp_dat, lubridate::year(date) %in% highest_year$year)

    if (nrow(temp_dat) > 0) {
      p2 <- ggplot(filter(temp_dat, model%in% 'temp_c'), aes(x = date, y = temp_c)) +
        geom_point(alpha = 0.7, size = 0.5) +
        geom_line(data = filter(temp_dat, !model%in% 'temp_c'),
                  aes(x = date, y = temp_c, color = model), alpha = 0.7) +
        facet_wrap(~type_res, ncol = 1) +
        theme_bw() +
        labs(x = '', y = 'Water temp [deg C]', title = res_name)

      ggsave(file.path('out', paste0('pred_observed_test_highlyobs_', reservoirs[i], '.png')), p2, height = 8, width = 8)


      temp_dat$new_id <- paste(temp_dat$type_res, temp_dat$seg_id_nat, sep = ' - ')
      p3 <- ggplot(filter(temp_dat, model%in% 'temp_c'), aes(x = date, y = temp_c)) +
        geom_point(alpha = 0.7, size = 0.5) +
        geom_line(data = filter(temp_dat, !model%in% 'temp_c'),
                  aes(x = date, y = temp_c, color = model), alpha = 0.7) +
        facet_wrap(~new_id, ncol = 1) +
        theme_bw() +
        labs(x = '', y = 'Water temp [deg C]', title = res_name)

      ggsave(file.path('out', paste0('pred_observed_test_uniquesites_', reservoirs[i], '.png')), p3, height = 10, width = 8)
    }

    # plot training dat
    training_dat <-  filter(dat, seg_id_nat %in% reaches$seg_id_nat) %>%
      left_join(select(reaches, seg_id_nat, type_res)) %>%
      filter(date < '2004-10-01') %>%
      filter(!is.na(temp_c) & model %in% c('temp_c', 'sntemp_temp_c'))

    p_train <- ggplot() +
      geom_point(data = filter(training_dat, model %in% 'temp_c'),
                 aes(x = date, y = temp_c), size = 0.2, alpha = 0.5) +
      geom_line(data = filter(training_dat, model %in% 'sntemp_temp_c'),
                aes(x = date, y = temp_c), color = 'darkgray', alpha = 0.5) +
      facet_wrap(~type_res, ncol = 1) +
      theme_bw() +
      labs(x = '', y = 'Water temp [deg C]', title = res_name)

    ggsave(file.path('out', paste0('training_period_', reservoirs[i], '.png')), p_train, height = 10, width = 8)


  }

  # show 2019 data for selected reservoirs
  # neversink
  #rmse for each segment
  # neversink
}


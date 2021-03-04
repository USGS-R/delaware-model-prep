library(scipiper)
library(tidyverse)

#### Observations ####

get_inout_obs <- function(
  site_ids = c(),
  flow_ind = '2_observations/out/drb_discharge_daily_dv.csv.ind', # /out/, not /in/? But I don't see a recipe for it so assume it's from Jeff
  temp_ind = '2_observations/in/daily_temperatures.rds.ind') {

  # read raw data
  flow_raw <- sc_retrieve(flow_ind, 'getters.yml') %>%
    readr::read_csv(col_types=cols(datetime='D', .default='d'))
  temp_raw <- sc_retrieve(temp_ind, 'getters.yml') %>%
    read_rds()

  # filter and format data for these sites
  flows <- flow_raw %>%
    select(datetime, any_of(site_ids)) %>%
    # is it CFS? or CMS, or other?
    pivot_longer(-datetime, names_to='site_id', values_to='flow_cms') %>%
    select(site_id, date = datetime, flow_cms) %>%
    filter(!is.na(flow_cms))
  temps <- temp_raw %>%
    filter(site_id %in% paste0('USGS-', site_ids)) %>%
    mutate(site_id = gsub('USGS-', '', site_id)) %>%
    select(site_id, date, temp_degC) %>%
    filter(!is.na(temp_degC))
  site_dates <- crossing(
    site_id = site_ids,
    date = seq(
      min(min(flows$date), min(temps$date)),
      max(max(flows$date), max(temps$date)),
      by=as.difftime(1, units='days')))

  # merge flows and temps, adding NAs for unobserved dates within a complete date sequence
  all_dat <- full_join(flows, temps, by=c('site_id','date')) %>%
    full_join(site_dates, by=c('site_id','date'))

  return(all_dat)
}

# TODO: read and subset the full flow and temp files just once, not once per reservoir, b/c it's slow

get_inout_obs_cannonsville <- function() {
  res_id <- 'nhdhr_120022743'
  inflow_sites <- c('0142400103', '01423000')
  # res_sites <- '01423910'
  outflow_sites <- '01425000'

  inouts_raw <- get_inout_obs(c(inflow_sites, outflow_sites))
  inouts <- inouts_raw %>%
    mutate(
      location = case_when(
        site_id %in% inflow_sites ~ 'inflow',
        TRUE ~ 'outflow'),
      res_id = res_id)

  # visualize
  ggplot(inouts, aes(x=date, y=flow_cms, color=location)) + geom_line() + facet_grid(site_id ~ .) + ggtitle('Cannonsville Discharge Data')
  ggsave('2_observations/tmp/can_io_flow.png', height=3)
  ggplot(inouts, aes(x=date, y=temp_degC, color=location)) + geom_point(size=0.2) + facet_grid(site_id ~ .) + ggtitle('Cannonsville Temperaturature Data')
  ggsave('2_observations/tmp/can_io_temp.png', height=3)

  # TODO: interpolate inflows at 014200103

  # return
  inouts
}
get_inout_obs_pepacton <- function() {
  res_id <- 'nhdhr_151957878'
  inflow_sites <- c('01415460', '01415000', '01414500', '01414000', '01413500') # no flow data and not much temp data in 01415460 though
  # res_sites <- '01414750'
  outflow_sites <- c('01417000', '01417500') # to merge

  inouts_raw <- get_inout_obs(c(inflow_sites, outflow_sites))
  inouts <- inouts_raw %>%
    mutate(
      location = case_when(
        site_id %in% inflow_sites ~ 'inflow',
        TRUE ~ 'outflow'),
      res_id = res_id)

  # visualize
  ggplot(inouts, aes(x=date, y=flow_cms, color=location)) + geom_line() + facet_grid(site_id ~ .) + ggtitle('Pepacton Discharge Data')
  ggsave('2_observations/tmp/pep_io_flow.png', height=7)
  ggplot(inouts, aes(x=date, y=temp_degC, color=location)) + geom_point(size=0.2) + facet_grid(site_id ~ .) + ggtitle('Pepacton Temperaturature Data')
  ggsave('2_observations/tmp/pep_io_temp.png', height=7)

  # TODO: interpolate the inflows (esp. 01414500)
  # (how much does GLM differ depending on whether you provide several individual
  # inflows or one lumped inflow? i suspect this changes how much the river
  # plunges, but maybe not)


  # TODO: merge the two outflow datasets, but it seems that just 01417500 covers everything we need from 1980 on
  inouts <- inouts %>%
    filter(site_id != '01417000')

  # temperature really isn't available as a timeseries at any site except the outflow

  # return
  inouts
}

merge_res_inouts <- function(out_file, tibbles) {
  bind_rows(tibbles) %>%
    arrow::write_feather(out_file)
}

# run the functions defined above
can_io <- get_inout_obs_cannonsville()
pep_io <- get_inout_obs_pepacton()
merge_res_inouts(out_file = '9_collaborator_data/res/res_io_obs.feather', list(can_io, pep_io))

#### SNTemp Predictions ####

get_inout_sntemp <- function(res_id, inflow_segs, outflow_segs) {
  # use raw-ish SNTemp output
  preds <- arrow::read_feather('3_predictions/out/uncal_sntemp_input_output.feather') %>%
    filter(seg_id_nat %in% c(inflow_segs, outflow_segs)) %>%
    select(seg_id_nat, date, seg_outflow, seg_tave_water)

  if(!dir.exists('9_collaborator_data/res')) dir.create('9_collaborator_data/res')
  preds %>%
    mutate(
      direction = case_when(
        seg_id_nat %in% inflow_segs ~ 'inflow',
        TRUE ~ 'outflow'),
      res_id = res_id) %>%
    return()
}
can_io_sntemp <- get_inout_sntemp( # cannonsville
  res_id = 'nhdhr_120022743',
  inflow_segs = c('1559','1557'),
  # res_segs = c('1561','1560','1562'),
  outflow_segs = '1566')
pep_io_sntemp <- get_inout_sntemp( # pepacton
  res_id = 'nhdhr_151957878',
  inflow_segs = c('1440','1441','1443','1437'),
  # res_segs = c('1449','1447','1448','1446','1445','1438'),
  outflow_segs = '1444')
merge_res_inouts(out_file = '9_collaborator_data/res/res_io_sntemp.feather', tibbles=list(can_io_sntemp, pep_io_sntemp))

#### Exploration ####

# get_reservoir_inouts <- function(
#   out_ind = '2_observations/out/drb_reservoir_inouts.rds.ind',
#   res_outflow_ids = list(Cannonsville = '01425000', Pepacton = c('01417000','01417500')),
#   flow_ind = '2_observations/out/drb_discharge_daily_dv.csv.ind', # /out/, not /in/? But I don't see a recipe for it so assume it's from Jeff
#   temp_ind = '2_observations/in/daily_temperatures.rds.ind'
# ) {
#
#   # read in the available data; do this early so we know which sites have data potential
#   flow_dat <- readr::read_csv(sc_retrieve(flow_ind, 'getters.yml'), col_types=cols(datetime='D', .default='d'))
#   temp_dat <- readRDS(sc_retrieve(temp_ind, 'getters.yml'))
#   flow_sites <- setdiff(names(flow_dat), 'datetime')
#   temp_sites <- temp_dat %>% group_by(site_id) %>% tally() %>% filter(n > 1000) %>% pull(site_id) %>% unique()
#
#   # Use NLDI to algorithmically pick out inflows
#   # http://usgs-r.github.io/dataRetrieval/reference/findNLDI.html
#   # as of 2/2/21, NLDI requires remotes::install_gitlab('water/dataRetrieval', host='code.usgs.gov', auth_token='your_code.usgs.gov_token')
#   # max_dist_km <- 100
#   # res_inflow_ids <- purrr::map(res_outflow_ids, function(outflow_ids) {
#   #   upstream_ids <- purrr::map(outflow_ids, function(outflow_id) {
#   #     upstream_ids_list <- dataRetrieval::findNLDI(nwis = outflow_id, find='nwissite', nav='UT', distance_km = max_dist_km, no_sf=TRUE)
#   #     setdiff(upstream_ids_list$UT_nwissite$identifier, upstream_ids_list$origin$identifier) %>%
#   #       intersect(union(flow_sites, temp_sites))
#   #   }) %>% unlist() %>% unique() %>% setdiff(outflow_ids)
#   #   message(sprintf('### UPSTREAM OF %s: %s', paste(outflow_ids, collapse=' & '), paste(sort(upstream_ids), collapse=', ')))
#   #   if(length(upstream_ids) == 0) message('  found no data-rich upstream sites')
#   #   first_upstream <- purrr::map_lgl(upstream_ids, function(upstream_id) {
#   #     downstream_ids_list <- dataRetrieval::findNLDI(nwis = upstream_id, find='nwissite', nav='DM', distance_km = max_dist_km)
#   #     downstream_ids <- setdiff(downstream_ids_list$DM_nwissite$identifier, downstream_ids_list$origin$identifier)
#   #     # message(sprintf('%s is upstream of: %s', upstream_id, paste(sort(downstream_ids), collapse=', ')))
#   #     is_first_upstream <- !any(downstream_ids %in% upstream_ids)
#   #     message(sprintf('  %s: %s is directly upstream', is_first_upstream, upstream_id), appendLF = is_first_upstream)
#   #     if(!is_first_upstream) {
#   #       message(sprintf('; interrupted by %s', paste(downstream_ids[downstream_ids %in% upstream_ids], collapse=', ')))
#   #     }
#   #     return(is_first_upstream)
#   #   })
#   #   return(upstream_ids[first_upstream])
#   # })
#   # Frozen version of the above (NLDI can sometimes take a while, maybe 1-2 minutes for 2 reservoirs):
#   res_inflow_ids <- list(
#     Cannonsville = c("0142400103", "01423000"), # Cannonsville, 01425000
#     Pepacton = c("01413500", "01415000", "01414000", "01414500")) # Pepacton, 01417000. there's also 01415460, but mayber no data there
#
#   # pepacton outflow: best to combine 01417000 and 01417500 (500 is further
#   # downstream while 000 is right at the outlet; 500 started earlier and has
#   # more temperature data; 000 has the only flow data in the 1970s
#   flow_dat %>% select(datetime, all_of(res_outflow_ids$Pepacton)) %>% pivot_longer(-datetime) %>% filter(datetime > as.Date('1935-01-01')) %>% ggplot(aes(x=datetime, y=value, color=name)) + geom_line() + facet_grid(name ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Pepacton outflow discharges')
#   ggsave('2_observations/tmp/pep_out_flow.png', height=4)
#   flow_dat %>% select(datetime, all_of(res_outflow_ids$Pepacton)) %>% ggplot(aes(x=`01417000`, y=`01417500`)) + geom_abline() + geom_point() + theme_bw() + ggtitle('Pepacton outflow discharge relationship')
#   ggsave('2_observations/tmp/pep_out_flow_paired.png', width=5, height=5)
#   temp_dat %>% filter(site_id %in% sprintf('USGS-%s', res_outflow_ids$Pepacton)) %>% ggplot(aes(x=date, y=temp_degC, color=site_id)) + geom_point(size=0.1) + facet_grid(site_id ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle("Pepacton outflow temperatures")
#   ggsave('2_observations/tmp/pep_out_temp.png', height=4)
#   # pepacton inflows
#   flow_dat %>% select(datetime, all_of(res_inflow_ids$Pepacton)) %>% pivot_longer(-datetime) %>% filter(datetime > as.Date('1935-01-01')) %>% ggplot(aes(x=datetime, y=value, color=name)) + geom_line() + facet_grid(name ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Pepacton inflow discharges')
#   ggsave('2_observations/tmp/pep_in_flow.png', height=7)
#   temp_dat %>% filter(site_id %in% sprintf('USGS-%s', c(res_inflow_ids$Pepacton))) %>% ggplot(aes(x=date, y=temp_degC, color=site_id)) + geom_point(size=0.1) + facet_grid(site_id ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Pepacton inflow temperatures')
#   ggsave('2_observations/tmp/pep_in_temp.png', height=4)
#
#   # cannonsville outflows
#   flow_dat %>% select(datetime, all_of(res_outflow_ids$Cannonsville)) %>% pivot_longer(-datetime) %>% filter(datetime > as.Date('1935-01-01')) %>% ggplot(aes(x=datetime, y=value, color=name)) + geom_line() + facet_grid(name ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Cannonsville outflow discharges')
#   ggsave('2_observations/tmp/can_out_flow.png', height=2.5)
#   temp_dat %>% filter(site_id %in% sprintf('USGS-%s', res_outflow_ids$Cannonsville)) %>% ggplot(aes(x=date, y=temp_degC, color=site_id)) + geom_point(size=0.1) + facet_grid(site_id ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle("Cannonsville outflow temperatures")
#   ggsave('2_observations/tmp/can_out_temp.png', height=2.5)
#   # inflows
#   flow_dat %>% select(datetime, all_of(res_inflow_ids$Cannonsville)) %>% pivot_longer(-datetime) %>% filter(datetime > as.Date('1935-01-01')) %>% ggplot(aes(x=datetime, y=value, color=name)) + geom_line() + facet_grid(name ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Cannonsville inflow discharges')
#   ggsave('2_observations/tmp/can_in_flow.png', height=4)
#   temp_dat %>% filter(site_id %in% sprintf('USGS-%s', res_inflow_ids$Cannonsville)) %>% ggplot(aes(x=date, y=temp_degC, color=site_id)) + geom_point(size=0.1) + facet_grid(site_id ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Cannonsville inflow temperatures')
#   ggsave('2_observations/tmp/can_in_temp.png', height=2.5)
# }

# get_reservoir_inouts_pgdl <- function() {
#   # rgcn_v2 goes for 5.5 years
#   scipiper::gd_get('3_predictions/in/rgcn_v2_preds.feather.ind')
#   rgcn2 <- arrow::read_feather('3_predictions/in/rgcn_v2_preds.feather')
#   range(rgcn2$date) # "2004-11-12 19:00:00 EST" "2010-05-12 20:00:00 EDT"
#
#   # same date range for rgcn2_full in compare_predictions_obs.feather
#   gd_get('3_predictions/out/compare_predictions_obs.feather.ind')
#   pgdl_preds <- arrow::read_feather('3_predictions/out/compare_predictions_obs.feather')
#   rgcn_preds <- pgdl_preds %>% select(seg_id_nat, date, sntemp_temp_c, rgcn2_full_temp_c, obs_temp_c = temp_c, site_id) %>%
#     pivot_longer(cols=sntemp_temp_c:obs_temp_c, names_to='model', values_to='temp_c') %>%
#     filter(!is.na(temp_c)) %>%
#     mutate(model = gsub('_temp_c', '', model))
#
#   rgcn_preds %>%
#     filter(model == 'rgcn2_full') %>%
#     group_by(seg_id_nat) %>%
#     summarize(min(date), max(date))
#
#   rgcn_preds %>%
#     filter(model == 'rgcn2_full', seg_id_nat == '2048') %>%
#     ggplot(aes(x=date, y=temp_c)) + geom_line()
# # }

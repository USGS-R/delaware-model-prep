#### Observations ####

#' @param res_outflow_ids named list where each element is a vector of 1 or more NWIS site IDs just downstream of the reservoir (above which any stream sites can be assumed to be tributaries)
#' @param flow_indpath of the .ind file of the smallest, most munged file available that still has all the discharge observation sites in DRB (or beyond)
#' @param temp_ind path of the .ind file of the smallest, most munged file available that still has all the temperature observation sites in DRB
#' @param min_obs the minimum number of observations (of flow or temperature) that a site must have before we'll consider it
find_inout_obs_sites <- function(
  res_outflow_ids = list(Cannonsville = '01425000', Pepacton = c('01417000','01417500')),
  flow_ind = '2_observations/in/daily_flow.rds.ind', # '2_observations/out/drb_discharge_daily_dv.csv.ind',
  temp_ind = '2_observations/out/all_drb_temp_obs.rds.ind', # '2_observations/in/daily_temperatures.rds.ind',
  min_obs_flow = 1000,
  min_obs_temp = 50,
  max_dist_km = 100) {

  # read in the available data; do this early so we know which sites have data potential (i.e., at least 1000 obs)
  #flow_dat <- readr::read_csv(sc_retrieve(flow_ind, 'getters.yml'), col_types=cols(datetime='D', .default='d'))
  flow_dat <- sc_retrieve(flow_ind, 'getters.yml') %>% read_rds()
  temp_dat <- sc_retrieve(temp_ind, 'getters.yml') %>% read_rds()
  flow_sites <- flow_dat %>% group_by(site_id) %>% tally() %>% filter(n >= min_obs_flow) %>% pull(site_id) %>% unique()
  temp_sites <- temp_dat %>% group_by(site_id) %>% tally() %>% filter(n >= min_obs_temp & grepl('^USGS-', site_id)) %>%
    mutate(site_id = gsub('USGS-', '', site_id)) %>% pull(site_id) %>% unique()
  obs_sites <- list('flow'=flow_sites, 'temp'=temp_sites)

  # Use NLDI to algorithmically pick out inflows. Choose those sites that meet the
  # min_obs criterion above and are closest to the reservoir along a given upstream
  # route (i.e., don't choose two sites on the same inflow tributary).
  # NLDI: http://usgs-r.github.io/dataRetrieval/reference/findNLDI.html.
  # As of 2/2/21, dataRetrieval access to NLDI requires
  # remotes::install_gitlab('water/dataRetrieval', host='code.usgs.gov', auth_token='your_code.usgs.gov_token')
  res_inflow_ids <- purrr::map(res_outflow_ids, function(outflow_ids) {
    purrr::map(setNames(nm=c('flow','temp')), function(variable) {

      # find all reasonable candidate sites within the specified maximum river distance from the outlet
      upstream_ids <- purrr::map(outflow_ids, function(outflow_id) {
        upstream_ids_list <- dataRetrieval::findNLDI(
          nwis = outflow_id, find='nwissite', nav='UT', distance_km = max_dist_km, no_sf=TRUE)
        setdiff(upstream_ids_list$UT_nwissite$identifier, upstream_ids_list$origin$identifier) %>%
          intersect(obs_sites[[variable]])
      }) %>% unlist() %>% unique() %>% setdiff(outflow_ids)
      message(sprintf('### UPSTREAM OF %s: %s', paste(outflow_ids, collapse=' & '), paste(sort(upstream_ids), collapse=', ')))
      if(length(upstream_ids) == 0) message('  found no data-rich upstream sites')

      first_upstream <- purrr::map_lgl(upstream_ids, function(upstream_id) {
        downstream_ids_list <- dataRetrieval::findNLDI(
          nwis = upstream_id, find='nwissite', nav='DM', distance_km = max_dist_km)
        downstream_ids <- setdiff(downstream_ids_list$DM_nwissite$identifier, downstream_ids_list$origin$identifier)
        # message(sprintf('%s is upstream of: %s', upstream_id, paste(sort(downstream_ids), collapse=', ')))
        is_first_upstream <- !any(downstream_ids %in% upstream_ids)
        message(sprintf('  %s: %s is directly upstream', is_first_upstream, upstream_id), appendLF = is_first_upstream)
        if(!is_first_upstream) {
          message(sprintf('; interrupted by %s', paste(downstream_ids[downstream_ids %in% upstream_ids], collapse=', ')))
        }
        return(is_first_upstream)
      })
      return(upstream_ids[first_upstream])
    })
  })
}

#' Read in the flow and temp data, subset to site_ids, and do a bit of munging
get_inout_obs <- function(
  site_ids = c(),
  flow_ind = '2_observations/in/daily_flow.rds.ind', # '2_observations/out/drb_discharge_daily_dv.csv.ind',
  temp_ind = '2_observations/out/all_drb_temp_obs.rds.ind' # '2_observations/in/daily_temperatures.rds.ind',
) {

  # read raw data
  # flow_raw <- sc_retrieve('2_observations/out/drb_discharge_daily_dv.csv.ind', 'getters.yml') %>%
  #   readr::read_csv(col_types=cols(datetime='D', .default='d'))
  flow_raw <- sc_retrieve(flow_ind, 'getters.yml') %>% read_rds()
  temp_raw <- sc_retrieve(temp_ind, 'getters.yml') %>% read_rds()

  # filter and format data for these sites
  # flows <- flow_raw %>%
  #   select(datetime, any_of(site_ids)) %>%
  #   # is it CFS? or CMS, or other?
  #   pivot_longer(-datetime, names_to='site_id', values_to='flow_cms') %>%
  #   select(site_id, date = datetime, flow_cms) %>%
  #   filter(!is.na(flow_cms))
  flows <- flow_raw %>%
    filter(site_id %in% site_ids) %>%
    mutate(flow_cms = flow_cfs / 35.314666) %>%
    select(-flow_cfs)
  temps <- temp_raw %>%
    filter(site_id %in% paste0('USGS-', site_ids)) %>%
    mutate(site_id = gsub('USGS-', '', site_id))
  site_dates <- tidyr::crossing(
    site_id = site_ids,
    date = seq(
      min(min(flows$date), min(temps$date)),
      max(max(flows$date), max(temps$date)),
      by=as.difftime(1, units='days')))

  # merge flows and temps, adding NAs for unobserved dates within a complete date sequence
  all_dat <- full_join(flows, temps, by=c('site_id','date'), suffix=c('_flow','_temp')) %>%
    full_join(site_dates, by=c('site_id','date'))

  return(all_dat)
}

#' Subset `inouts_raw` for one reservoir, attach `location` column, write two figures to
#' '9_collaborator_data/res/%s_io_%s.png', and return the subsetted data
get_inout_obs_one <- function(inouts_raw, res_name, res_abbv, res_outflow_ids, res_inflow_ids) {
  # subset the in/out data and label as inflow or outflow. Do this here rather
  # than when creating inouts_raw just in case someday we have one reservoir
  # flowing almost directly into another one, in which case a reach could be an
  # outflow from one but inflow to another reservoir. Unlikely, I know, but I've
  # already written it this way
  res_io_ids <- unlist(c(res_inflow_ids, res_outflow_ids), recursive=TRUE)
  inouts <- inouts_raw %>%
    filter(site_id %in% res_io_ids) %>%
    mutate(
      location = case_when(
        site_id %in% res_outflow_ids ~ 'outflow',
        TRUE ~ 'inflow'))

  # visualize and save to figures
  inouts %>%
    filter(site_id %in% c(res_inflow_ids$flow, res_outflow_ids)) %>%
    ggplot(aes(x=date, y=flow_cms, color=location)) + geom_line() + facet_grid(site_id ~ .) +
    ggtitle(sprintf('%s Discharge Data', res_name))
  ggsave(sprintf('9_collaborator_data/res/%s_io_flow.png', res_abbv), height=length(res_io_ids))
  inouts %>%
    filter(site_id %in% c(res_inflow_ids$temp, res_outflow_ids)) %>%
    ggplot(aes(x=date, y=temp_degC, color=location)) + geom_point(size=0.2) + facet_grid(site_id ~ .) +
    ggtitle(sprintf('%s Temperature Data', res_name))
  ggsave(sprintf('9_collaborator_data/res/%s_io_temp.png', res_abbv), height=length(res_io_ids))

  # return
  inouts
}


# Wrapper function for getting in/observations for all sites. This function
# reads and subsets the full flow and temp files just once, not once per reservoir, b/c it's slow.
# There's a unique function for each reservoir because they require some custom handling, and
# could require more in the future, but then those outputs are recombined into a single feather file
# to serve as a scipiper target.
get_inout_obs_all <- function(out_file = '9_collaborator_data/res/res_io_obs.feather', res_inflow_ids, res_outflow_ids) {

  # Read in, munge, and coarsely subset the inflow-outflow observations once for all reservoirs
  res_all_site_ids <- sort(unique(unlist(c(res_inflow_ids, res_outflow_ids), recursive=TRUE)))
  inouts_raw <- get_inout_obs(res_all_site_ids)

  # Do reservoir-specific processing. (I separated these calls by reservoir
  # because I thought there'd be more custom processing...not a whole lot right
  # now, but could still become a need.)
  can_io <- get_inout_obs_one(
    inouts_raw,
    res_name = 'Cannonsville',
    res_abbv = 'can',
    res_outflow_ids$Cannonsville,
    res_inflow_ids$Cannonsville)
  pep_io <- get_inout_obs_one(
    inouts_raw,
    res_name = 'Pepacton',
    res_abbv = 'pep',
    res_outflow_ids$Pepacton,
    res_inflow_ids$Pepacton) %>%
    filter(site_id != '01417000') # we started with two outflow sites, but after plotting in the function above, we don't need both any more

  # Join all the reservoir data and write to file
  bind_rows(
    nhdhr_120022743 = can_io,
    nhdhr_151957878 = pep_io,
    .id = 'res_id') %>%
    arrow::write_feather(out_file)
}

#### SNTemp Predictions ####

get_inout_sntemp <- function(inouts_raw, inflow_segs, outflow_segs) {
  inouts_raw %>%
    filter(seg_id_nat %in% c(inflow_segs, outflow_segs)) %>%
    mutate(
      direction = case_when(
        seg_id_nat %in% outflow_segs ~ 'outflow',
        TRUE ~ 'inflow')) %>%
    return()
}

get_inout_sntemp_all <- function(out_file = '9_collaborator_data/res/res_io_sntemp.feather') {

  # read in the raw-ish SNTemp output
  preds <- arrow::read_feather('3_predictions/out/uncal_sntemp_input_output.feather') %>%
    select(seg_id_nat, date, seg_outflow, seg_tave_water)

  # do reservoir-specific processing. I used the DRB internal viz to identify
  # the inflow and outflow segments for each reservoir
  can_io <- get_inout_sntemp( # cannonsville
    inouts_raw = preds,
    inflow_segs = c('1559','1557'),
    # res_segs = c('1561','1560','1562'),
    outflow_segs = '1566')
  pep_io <- get_inout_sntemp( # pepacton
    inouts_raw = preds,
    inflow_segs = c('1440','1441','1443','1437'),
    # res_segs = c('1449','1447','1448','1446','1445','1438'),
    outflow_segs = '1444')

  # Combine and write the data to a single outfile
  bind_rows(
    nhdhr_120022743 = can_io,
    nhdhr_151957878 = pep_io,
    .id = 'res_id') %>%
    arrow::write_feather(out_file)
}

#### PGDL Predictions ####

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

# http://usgs-r.github.io/dataRetrieval/reference/findNLDI.html
# as of 2/2/21, NLDI requires remotes::install_gitlab('water/dataRetrieval', host='code.usgs.gov', auth_token='your_code.usgs.gov_token')
get_reservoir_inouts <- function(
  res_outflow_ids = list(Cannonsville = '01425000', Pepacton = c('01417000','01417500')),
  flow_ind = '2_observations/out/drb_discharge_daily_dv.csv.ind', # /out/, not /in/? But I don't see a recipe for it so assume it's from Jeff
  temp_ind = '2_observations/in/daily_temperatures.rds.ind'
) {

  # read in the available data; do this early so we know which sites have data potential
  flow_dat <- readr::read_csv(sc_retrieve(flow_ind, 'getters.yml'), col_types=cols(datetime='D', .default='d'))
  temp_dat <- readRDS(sc_retrieve(temp_ind, 'getters.yml'))
  flow_sites <- setdiff(names(flow_dat), 'datetime')
  temp_sites <- temp_dat %>% group_by(site_id) %>% tally() %>% filter(n > 1000) %>% pull(site_id) %>% unique()

  max_dist_km <- 100
  res_inflow_ids <- purrr::map(res_outflow_ids, function(outflow_ids) {
    upstream_ids <- purrr::map(outflow_ids, function(outflow_id) {
      upstream_ids_list <- dataRetrieval::findNLDI(nwis = outflow_id, find='nwissite', nav='UT', distance_km = max_dist_km, no_sf=TRUE)
      setdiff(upstream_ids_list$UT_nwissite$identifier, upstream_ids_list$origin$identifier) %>%
        intersect(union(flow_sites, temp_sites))
    }) %>% unlist() %>% unique() %>% setdiff(outflow_ids)
    message(sprintf('### UPSTREAM OF %s: %s', paste(outflow_ids, collapse=' & '), paste(sort(upstream_ids), collapse=', ')))
    if(length(upstream_ids) == 0) message('  found no data-rich upstream sites')
    first_upstream <- purrr::map_lgl(upstream_ids, function(upstream_id) {
      downstream_ids_list <- dataRetrieval::findNLDI(nwis = upstream_id, find='nwissite', nav='DM', distance_km = max_dist_km)
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
  # Frozen version of the above (NLDI can sometimes take a while):
  # res_inflow_ids <- list(
  #   Cannonsville = c("0142400103", "01423000"), # Cannonsville, 01425000
  #   Pepacton = c("01413500", "01415000", "01414000", "01414500")) # Pepacton, 01417000

  # pepacton outflow: best to combine 01417000 and 01417500 (500 is further
  # downstream while 000 is right at the outlet; 500 started earlier and has
  # more temperature data; 000 has the only flow data in the 1970s
  flow_dat %>% select(datetime, all_of(res_outflow_ids$Pepacton)) %>% pivot_longer(-datetime) %>% filter(datetime > as.Date('1935-01-01')) %>% ggplot(aes(x=datetime, y=value, color=name)) + geom_line() + facet_grid(name ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Pepacton outflow discharges')
  ggsave('2_observations/tmp/pep_out_flow.png')
  flow_dat %>% select(datetime, all_of(res_outflow_ids$Pepacton)) %>% ggplot(aes(x=`01417000`, y=`01417500`)) + geom_abline() + geom_point() + theme_bw() + ggtitle('Pepacton outflow discharge relationship')
  ggsave('2_observations/tmp/pep_out_flow_paired.png')
  temp_dat %>% filter(site_id %in% sprintf('USGS-%s', res_outflow_ids$Pepacton)) %>% ggplot(aes(x=date, y=temp_degC, color=site_id)) + geom_line() + facet_grid(site_id ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle("Pepacton outflow temperatures")
  ggsave('2_observations/tmp/pep_out_temp.png')
  # pepacton inflows
  flow_dat %>% select(datetime, all_of(res_inflow_ids$Pepacton)) %>% pivot_longer(-datetime) %>% filter(datetime > as.Date('1935-01-01')) %>% ggplot(aes(x=datetime, y=value, color=name)) + geom_line() + facet_grid(name ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Pepacton inflow discharges')
  ggsave('2_observations/tmp/pep_in_flow.png')
  temp_dat %>% filter(site_id %in% sprintf('USGS-%s', c(res_outflow_ids$Pepacton), res_inflow_ids$Pepacton)) %>% ggplot(aes(x=date, y=temp_degC, color=site_id)) + geom_point(size=0.1) + facet_grid(site_id ~ .) + scale_color_discrete(guide=FALSE) + theme_bw() + ggtitle('Pepacton inflow temperatures')
  ggsave('2_observations/tmp/pep_in_temp.png')

  flow_dat %>%
    select(datetime, all_of(unname(res_outflow_ids))) %>%
    filter(!is.na(.[2]) | !is.na(.[3]))
}

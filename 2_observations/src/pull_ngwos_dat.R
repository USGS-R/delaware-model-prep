retrieve_ngwos <- function(out_ind, sites_ind) {
  # get latest NGWOS data
  ngwos_meta <- readxl::read_xlsx(
    sc_retrieve(sites_ind, 'getters.yml'), sheet=1, na=c('','NA'),
    col_types=replace(rep('text', 20), 13:17, 'numeric'))

  # clean up site IDs
  # some listed as TBD and should be removed
  temp_sites <- filter(ngwos_meta, !is.na(Temp) & !(ID %in% 'TBD'))

  # pull out just the numbers w/o quotes
  temp_sites$site_id <- gsub('([[:punct:]]*)(\\d*)([[:punct:]]*)', '\\2', temp_sites$ID, perl = TRUE)

  # fix one long site number that got turned into sci notation in Excel
  temp_sites$site_id[temp_sites$site_id == '403447E14'] <- '403447075331801'

  # remove duplicates
  temp_sites <- select(temp_sites, site_id) %>% distinct()


  # filter all data to NGWOS sites
  all_dat <- readRDS(sc_retrieve('2_observations/out/all_drb_temp_obs.rds.ind', 'getters.yml'))
  ngwos_dat <- filter(all_dat, gsub('USGS-', '', site_id) %in% temp_sites$site_id) %>%
    filter(source %in% c('nwis_uv', 'nwis_dv'))

  # retrieve site metadata from NWIS
  nwis_sites <- dataRetrieval::readNWISsite(temp_sites$site_id)

  # did any sites fail to be recognized by NWIS? Yes
  nwis_fail <- setdiff(temp_sites$site_id, nwis_sites$site_no)
  if(length(nwis_fail) == 0) {
    stop(sprintf("These NGWOS sites were not recognized by NWIS: %s", paste(nwis_fail, collapse=', ')))
  }

  # keep only stream sites - drops 3 lakes, 1 estuary, 1 spring
  stream_sites <- filter(nwis_sites, grepl('ST', site_tp_cd))

  ngwos_dat <- filter(ngwos_dat, gsub('USGS-', '', site_id) %in% stream_sites$site_no)

  sites_date <- ngwos_dat %>%
    group_by(site_id) %>%
    summarize(min_year = min(lubridate::year(date)),
              max_year = max(lubridate::year(date)),
              n_overall = n(),
              n_after_ngwos = length(date[date > as.Date('2017-10-01')])) %>%
    mutate(site_id = gsub('USGS-', '', site_id))

  # any sites that were found but missing NGWOS data?
  # just two, and those are outside of the DRB
  sites_no_ngwos <- filter(sites_date, n_after_ngwos == 0) %>% pull(site_id)

  ## return NGWOS data
  saveRDS(ngwos_dat, as_data_file(out_ind))
  gd_put(out_ind)

}

# figure out which NGWOS sites to withhold
filter_ngwos <- function(ngwos_ind){

  dat <- readRDS(sc_retrieve(ngwos_ind, 'getters.yml'))

  summary <-  dat %>%
    group_by(site_id) %>%
    summarize(n_pre_NGWOS = length(date[date < as.Date('2017-10-01') & date > as.Date('2013-10-01')]),
              n_pre_all = length(date[date < as.Date('2017-10-01')]),
              n_post_NGWOS = length(date[date >= as.Date('2017-10-01')]))

  sites_potential_drop <- summary %>%
    filter(n_pre_NGWOS > 0)

  # some sites have data pre-NGWOS, but not that much. Maybe was just early install?
  # maybe should drop when continuous records start more recently instead of the 2017-10-01 cutoff?

  sites_keep <- summary %>%
    filter(n_pre_NGWOS < 450)

  message('Sites ', paste(summary$site_id[summary$n_pre_NGWOS >=450], collapse = ', '), ' were dropped due to being heavily monitored prior to NGWOS in the period of 2013-10-01 to 2017-10-01')

  # did any of these fail to return any post-NGWOS data?

  sites_keep <- sites_keep %>%
    filter(n_post_NGWOS > 0)

  if (sum(sites_keep$n_post_NGWOS == 0) > 0) {
    message('Sites ', paste(sites_keep$site_id[sites_keep$n_post_NGWOS ==0], collapse = ', '), ' were dropped due to having no data in the NGWOS period.')
  }

  keepers <- sites_keep$site_id

  return(keepers)

}

get_ngwos_reaches <- function(sites_ind, ngwos_sites) {
  sites <- readRDS(sc_retrieve(sites_ind, 'getters.yml'))

  sites_ngwos <- filter(sites, site_id %in% paste0('USGS-', ngwos_sites)) %>%
    select(site_id, seg_id_nat) %>% distinct()

  return(sites_ngwos$seg_id_nat)
}

explore_ngwos <- function() {
  # were any of these sites being monitored prior to NGWOS
  test <- filter(ngwos_dat, date > as.Date('2013-10-01'))

  # sites with considerable pre-NGWOS data
  date_summary <- test %>%
    group_by(site_id) %>%
    summarize(n_pre_NGWOS = length(date[date < as.Date('2017-10-01')]),
              n_post_NGWOS = length(date[date >= as.Date('2017-10-01')]))

  date_summary_all <- ngwos_dat %>%
    group_by(site_id) %>%
    summarize(n_pre_NGWOS = length(date[date < as.Date('2017-10-01') & date > as.Date('2013-10-01')]),
              n_pre_all = length(date[date < as.Date('2017-10-01')]),
              n_post_NGWOS = length(date[date >= as.Date('2017-10-01')]))

  test_prengwos <- filter(test, site_id %in% date_summary$site_id[date_summary$n_pre_NGWOS > 0])

  ggplot(test_prengwos, aes(x = date, y = temp_c)) +
    geom_point() +
    facet_wrap(~site_id) +
    geom_vline(aes(xintercept = as.Date('2017-10-01')), color = 'red')

  # sites I could not find
  temp_sites$site_id[which(!temp_sites$site_id %in% unique(ngwos_dat$site_id))]

  # are the ngwos data in the filtered sites?
  # first, are they in the site list at all?

  ngwos_sites <- unique(ngwos_dat$site_id)
  all_sites <- readRDS('2_observations/out/crosswalk_site_reach.rds')
  summary(ngwos_sites %in% gsub('USGS-', '', unique(all_sites$site_id)))
}

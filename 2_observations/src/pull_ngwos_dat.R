retrieve_ngwos <- function(out_ind, sites_ind) {
  # get latest NGWOS data
  
  
  ngwos_meta <- readxl::read_xlsx(sc_retrieve(sites_ind))
  
  # clean up site IDs
  # some listed as TBD and should be removed
  temp_sites <- filter(ngwos_meta, !is.na(Temp) & !(ID %in% 'TBD'))
  
  # pull out just the numbers w/o quotes
  temp_sites$site_id <- gsub('([[:punct:]]*)(\\d*)([[:punct:]]*)', '\\2', temp_sites$ID, perl = TRUE)
  
  # add leading zeros to those that are only 7 characters long
  temp_sites$site_id[nchar(temp_sites$site_id) == 7] <- paste0('0', temp_sites$site_id[nchar(temp_sites$site_id) == 7])
  
  # fix one long site number that got turned into sci notation in Excel
  temp_sites$site_id[nchar(temp_sites$site_id)==15] <- '403447075331801'
  
  # remove duplicates
  temp_sites <- select(temp_sites, site_id) %>% distinct()
  
  # retrieve site metadata from NWIS
  nwis_sites <- dataRetrieval::readNWISsite(temp_sites$site_id)
  
  # keep only stream sites - drops 3 lakes, 1 estuary, 1 spring
  stream_sites <- filter(nwis_sites, grepl('ST', site_tp_cd))
  
  # did any sites fail to be recognized by NWIS? Nope.
  nwis_fail <- temp_sites$site_id[!temp_sites$site_id %in% nwis_sites$site_no]
  
  # dv or uv?
  dataRetrieval::setAccess('internal')
  
  # first find data in DV
  new_ngwos_dat <- dataRetrieval::readNWISdv(siteNumbers = stream_sites$site_no, parameterCd = '00010')
  
  dv_dat <- new_ngwos_dat %>%
    mutate(temp_c = X_00010_00003) %>%
    mutate(temp_c = ifelse(is.na(temp_c), `X_..2.._00010_00003`, temp_c)) %>%
    mutate(temp_c = ifelse(is.na(temp_c), `X_.NGWOS._00010_00003`, temp_c)) %>%
    mutate(source = 'nwis_dv',
           n_obs = 1) %>%
    filter(!is.na(temp_c)) %>%
    select(site_id = site_no, date = Date, temp_c, n_obs, source)
  
  sites_date <- dv_dat %>%
    group_by(site_id) %>%
    summarize(min_year = min(lubridate::year(date)), 
              max_year = max(lubridate::year(date)),
              n_overall = n(),
              n_after_ngwos = length(date[date > as.Date('2017-10-01')]))
  
  # any sites that were found but missing NGWOS data?
  sites_no_ngwos <- filter(sites_date, n_after_ngwos == 0) %>% pull(site_id)
  
  # find sites we still do not have NGWOS data for, pass to UV
  missing_sites <- c(stream_sites$site_no[!stream_sites$site_no %in% sites_date$site_id], sites_no_ngwos)
  
  # retrieve remaining sites from NWISuv
  new_ngwos_uv <- dataRetrieval::readNWISuv(siteNumbers = missing_sites, parameterCd = '00010')
  
  uv_long <- select(new_ngwos_uv, site_no, dateTime, ends_with('00010_00000')) %>%
    tidyr::gather(key = 'temp_column', value = 'temp_c', - site_no, -dateTime)
  
  uv_site_col <- filter(uv_long, !is.na(temp_c)) %>%
    group_by(site_no, temp_column) %>%
    summarize(n_vals = n(),
              n_dates = length(unique(as.Date(dateTime)))) %>%
    filter(!grepl('piezometer', temp_column, ignore.case = TRUE))
  
  # always choose the standard temp column. In cases where that is missing, choose the one on that day
  # with the most data
  # first take day-temp type means
  uv_long_dailies <- filter(uv_long, !is.na(temp_c)) %>%
    filter(!grepl('piezometer', temp_column, ignore.case = TRUE)) %>%
    group_by(site_no, date = as.Date(dateTime), temp_column) %>%
    summarize(temp_c = mean(temp_c),
              n_obs = n()) %>%
    left_join(select(uv_site_col, site_no, temp_column, n_dates))
  
  # find the temperature for each site-day
  # first choose standard temp column, then choose one with most data when available
  uv_dat <- uv_long_dailies %>%
    group_by(site_no, date) %>%
    summarize(temp_c = ifelse(grepl('X_00010_00000', paste0(temp_column, collapse = ', ')), 
                              temp_c[which(temp_column %in% 'X_00010_00000')], temp_c[which.max(n_dates)]),
              temp_column = ifelse(grepl('X_00010_00000', paste0(temp_column, collapse = ', ')), 
                                   'X_00010_00000', temp_column[which.max(n_dates)]),
              n_obs = ifelse(grepl('X_00010_00000', paste0(temp_column, collapse = ', ')), 
                             n_obs[which(temp_column %in% 'X_00010_00000')], n_obs[which.max(n_dates)])) %>%
    mutate(source = 'nwis_uv') %>%
    select(site_id = site_no, date, temp_c, n_obs, source)
  
  ngwos_dat <- bind_rows(dv_dat, uv_dat)
  
  ## return NGWOS data
  saveRDS(ngwos_dat, as_data_file(out_ind))
  gd_put(out_ind)

}

# figure out which NGWOS sites to withhold
filter_ngwos <- function(ngwos_ind){

  dat <- readRDS(sc_retrieve(ngwos_ind))
  
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
  sites <- readRDS(sc_retrieve(sites_ind))

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





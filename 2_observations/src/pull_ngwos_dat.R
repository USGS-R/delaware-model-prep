retrieve_ngwos <- function(out_ind) {
  # get latest NGWOS data
  
  # these sites painstakingly retrieved from 
  # https://www.usgs.gov/mission-areas/water-resources/science/next-generation-water-observing-system-delaware-river-basin?qt-science_center_objects=1#qt-science_center_objects
  # note I only kept enhanced gages that mentioned temperature, which was confirmed by looking at the others which have
  # temperature data that predates NGWOS
  new_ngwos <-  c('01455300', '01464040', '01464598', '01483050', '01416900', '01419500', '01424997', '01427500', 
                  '01428000', '01435800', '01427200', '01432500', '01467200', '01471875', '01475510')
  
  enhanced_ngwos <- c('01478000', '01478650', '01479000', '01480000', '01438500', '01446500', '01457500', 
                      '01462000', '01413500', '01423000', '01427207', '01432160', '01434000', '01437500', 
                      '01431500', '01465500', '01467087', '01473000', '01474500', '01470500')
  
  # dv or uv?
  dataRetrieval::setAccess('internal')
  
  new_ngwos_dat <- dataRetrieval::readNWISdv(siteNumbers = new_ngwos, parameterCd = '00010') %>%
    mutate(site_id = paste0('USGS-', site_no), 
           source = 'nwis_dv',
           temp_degC = ifelse(is.na(X_00010_00003), X_.NGWOS._00010_00003, X_00010_00003),
           n_obs = 1) %>%
    select(site_id, date = Date, temp_degC, source)
  
  length(unique(new_ngwos_dat$site_id))
  enhanced_dat <- dataRetrieval::readNWISdv(siteNumbers = enhanced_ngwos, parameterCd = '00010') %>%
    mutate(site_id = paste0('USGS-', site_no), 
           source = 'nwis_dv',
           temp_degC = ifelse(is.na(X_00010_00003), `X_..2.._00010_00003`, X_00010_00003),
           n_obs = 1) %>%
    select(site_id, date = Date, temp_degC, source)
  
  length(unique(enhanced_dat$site_id))
  
  # check for missing sites on UV
  
  
  missing_sites <- c(new_ngwos[!new_ngwos %in% gsub('USGS-', '', unique(new_ngwos_dat$site_id))], 
                     enhanced_ngwos[!enhanced_ngwos %in% gsub('USGS-', '', unique(enhanced_dat$site_id))])
  
  ngwos_uv <- dataRetrieval::readNWISuv(siteNumbers = missing_sites, parameterCd = '00010') %>%
    mutate(date = as.Date(dateTime), 
           site_id = paste0('USGS-', site_no), source = 'nwis_uv') %>%
    select(site_id, date, temp_degC = 'X_00010_00000', source) %>%
    group_by(site_id, date, source) %>%
    summarize(temp_degC = mean(temp_degC), n_obs = n())
  
  all_dat <- bind_rows(ngwos_uv, new_ngwos_dat, enhanced_dat)
  
  saveRDS(all_dat, as_data_file(out_ind))
  gd_put(out_ind)
}

# figure out which NGWOS sites to withhold
filter_ngwos <- function(ngwos_ind){

  summary <- readRDS(sc_retrieve(ngwos_ind)) %>%
    group_by(site_id) %>%
    summarize(n_pre = sum(date < as.Date('2017-10-01')),
              n_post = sum(date >= as.Date('2017-10-01')))
  
  sites_keep <- summary %>%
    filter(n_pre == 0|n_post > n_pre)
  
  # site 0147500 had lots of pre data, but seemed to be out of commission from 2001-2019
  # and came back online in NGWOS days
  
  keepers <- c(sites_keep$site_id, 'USGS-01474500')
  
  # filter out sites that are not stream sites
  site_meta <- dataRetrieval::readNWISsite(gsub('USGS-', '', keepers)) %>%
    filter(site_tp_cd %in% c('ST', 'ST-TS')) # keep stream and tidal stream, drops 1 estuary site
  
  keepers <- keepers[gsub('USGS-', '', keepers) %in% site_meta$site_no]
  
  return(keepers)
  
}

get_ngwos_reaches <- function(sites_ind, ngwos_sites) {
  sites <- readRDS(sc_retrieve(sites_ind))
  
  sites_ngwos <- filter(sites, site_id %in% ngwos_sites) %>%
    filter(!source %in% 'wqp')
  
  return(sites_ngwos$seg_id_nat)
}





munge_rgcn_output <- function(in_ind, out_ind) {
  # put XJ's predictions in folder 3_predictions/in and read in
  dat_all <- npyLoad(sc_retrieve(in_ind))

  # transform and munge npy data
  munge_npy <- function(dat) {
    tdat <- as.data.frame(t(dat))
    names(tdat) <- c('2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012',
                     '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020',
                     '2021', '2022', '2023', '2024', '2025', '2026', '2027', '2028',
                     '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037',
                     '2038', '2039', '2040', '2041', '2044', '2045', '2046', '2047',
                     '2048', '4182')
    tdat$date <- seq.Date(from = as.Date('2004-10-01'), to = as.Date('2016-09-30'), by = 1)
    
    dat_long <- tidyr::gather(tdat, key = 'seg_id_nat', value = 'rgcn_temp_c', -date)
    return(dat_long)
  }
  
  rgcn <- munge_npy(dat_all)

  write_feather(rgcn, as_data_file(out_ind))
  gd_put(out_ind)
  
}

combine_preds_obs <- function(obs_ind, sntemp_ind, rgcn_v1_ind, rgcn_v2_ind, rgcn_v2_full_ind, out_ind){
  # join with SNTemp predictions
  sntemp <- read.csv(sc_retrieve(sntemp_ind), stringsAsFactors = FALSE) %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date)) %>% select(seg_id_nat, date, sntemp_temp_c)
  
  # bring in both rgcn mods
  # drop flow
  
  rgcn1 <- read_feather(sc_retrieve(rgcn_v1_ind))
  rgcn2 <- read_feather(sc_retrieve(rgcn_v2_ind)) %>%
    mutate(seg_id_nat = as.character(seg_id_nat), date = as.Date(date)) %>%
    rename(rgcn2_temp_c = temp_degC) %>% select(-discharge_cms)
  rgcn2_full <- read_feather(sc_retrieve(rgcn_v2_full_ind)) %>%
    mutate(seg_id_nat = as.character(seg_id_nat), date = as.Date(date)) %>%
    rename(rgcn2_full_temp_c = temp_degC) %>% select(-discharge_cms)

  
  preds <- left_join(sntemp, rgcn1) %>%
    left_join(rgcn2) %>%
    left_join(rgcn2_full)
  
  # bring in observations
  obs <- readRDS(sc_retrieve(obs_ind)) %>%
    mutate(seg_id_nat = as.character(seg_id_nat))
           
  preds_obs <- left_join(preds, select(obs, -subseg_id))
  
  write_feather(preds_obs, as_data_file(out_ind))
  gd_put(out_ind)
}

calc_metrics <- function(compare_ind, out_file) {
  
  compare <- read_feather(sc_retrieve(compare_ind))
  
  # make data long, filter where there are no observations
  # calculate error and squared error
  r_compare <- compare %>%
    pivot_longer(c(-seg_id_nat, -date, -temp_c), names_to = 'model', values_to = 'predicted') %>%
    filter(!is.na(predicted)) %>%
    filter(!is.na(temp_c)) %>%
    mutate(error = temp_c - predicted, sq_error = error^2) 
  
  # for now, find min and max dates to use as bounds on comparison
  # Jeff's model is setting the bounds as he's using a dev period
  # should draw from train, dev, test period in future
  dates <- group_by(r_compare, model) %>%
    summarize(start = min(date), end = max(date))
  
  start <- max(dates$start)
  end <- min(dates$end)
  
  # find segs in the subset we gave to XJ
  
  segs <- filter(r_compare, model %in% 'rgcn_temp_c') %>%
    select(seg_id_nat) %>% distinct() %>% pull(seg_id_nat)

  # calculate bias and RMSE
  stats_all <- filter(r_compare, date >= start & date <= end & seg_id_nat %in% segs) %>%
    group_by(model) %>%
    summarize(bias = round(mean(error), 2),
              rmse = round(sqrt(mean(sq_error)),2), 
              rmse_abv_20 = round(sqrt(mean(sq_error[temp_c > 20])),2), 
              rmse_blw_10 = round(sqrt(mean(sq_error[temp_c < 10])),2), 
              rmse_april = round(sqrt(mean(sq_error[lubridate::month(date) == 4])),2),
              rmse_july = round(sqrt(mean(sq_error[lubridate::month(date) == 7])),2),
              n = n()) %>%
    mutate(model = paste0(gsub('temp_c', '', model, ignore.case = TRUE), 'subset'))
  
  stats_all_full <- filter(r_compare, date >= start & date <= end) %>%
    group_by(model) %>%
    summarize(bias = round(mean(error), 2),
              rmse = round(sqrt(mean(sq_error)),2), 
              rmse_abv_20 = round(sqrt(mean(sq_error[temp_c > 20])),2), 
              rmse_blw_10 = round(sqrt(mean(sq_error[temp_c < 10])),2), 
              rmse_april = round(sqrt(mean(sq_error[lubridate::month(date) == 4])),2),
              rmse_july = round(sqrt(mean(sq_error[lubridate::month(date) == 7])),2),
              n = n()) %>%
    filter(model %in% c('sntemp_temp_c', 'rgcn2_full_temp_c')) %>%
    mutate(model = paste0(gsub('temp_c', '', model, ignore.case = TRUE), 'full'))
  
  out <- bind_rows(stats_all, stats_all_full)
  # write, but don't push to GD
  # going to git commit this one
  write.csv(out, out_file, row.names = FALSE)
  
}

pull_sntemp_preds <- function(sntemp_ind, out_ind) {
  # pull out predictions from sntemp in/out file from Jake
  dat <- feather::read_feather(sc_retrieve(sntemp_ind))
  preds <- select(dat, seg_id_nat, date, sntemp_temp_c = seg_tave_water)
  
  write.csv(preds, as_data_file(out_ind))
  gd_put(out_ind)
}

  

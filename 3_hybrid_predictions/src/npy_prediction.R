## creating a function that read data (npy files), process, and combine the data.

munge_npy_dat <- function(in_file, model, seg_file) {
  # to read npy data files from XJ's predictions (3_hyprid_prediction/in)
  dat_in <- npyLoad(in_file, 'getters.yml')

munge_npy_seg <- function(seg_file) {
  tdat <- npyLoad(seg_file, type = "integer")  # might need to transpose seg_file
  names(tdat) <- c('2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012',
                   '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020',
                   '2021', '2022', '2023', '2024', '2025', '2026', '2027', '2028',
                   '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037',
                   '2038', '2039', '2040', '2041', '2044', '2045', '2046', '2047',
                   '2048', '4182')
  tdat$date <- seq.Date(from = as.Date('2004-10-01'), to = as.Date('2016-09-30'), by = 1)

  dat_model <- tidyr::gather(tdat, key = 'seg_id_nat', value = model, -date)

  return(dat_model)
  }
}


combine_preds_obs <- function(obs_ind, sntemp_ind, ann_npy, rnn_npy, rgnc_npy, rgnc_ptnc_npy, out_file){
  # join with SNTemp predictions
  sntemp <- read.csv(sc_retrieve(sntemp_ind, 'getters.yml'), stringsAsFactors = FALSE) %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date)) %>% select(seg_id_nat, date, sntemp_temp_c)
  #bring in rgnc models using munge_npy_dat

  # 1) plain neural network
  ANN <- munge_npy_dat(in_file = ann_npy, model = 'ANN', seg_file = '3_hybrid_predictions/seg_ids.npy') # %>%
    #mutate(seg_id_nat = as.character(seg_id_nat), date = as.Date(date))

  # 2) + time
  RNN <- munge_npy_dat(in_file = rnn_npy, model = 'RNN', seg_file = '3_hybrid_predictions/seg_ids.npy') #%>%
   # mutate(seg_id_nat = as.character(seg_id_nat), date = as.Date(date))

  # 3) + space
  RGNC <- munge_npy_dat(in_file = rgnc_npy, model = 'RGNC', seg_file = '3_hybrid_predictions/seg_ids.npy') #%>%
   # mutate(seg_id_nat = as.character(seg_id_nat), date = as.Date(date))

  # 4) + pre_training
  RGNC_ptrn <- munge_npy_dat(in_file = rgnc_ptnc_npy, model = 'RGCN_ptrn', seg_file = '3_hybrid_predictions/seg_ids.npy')# %>%
   # mutate(seg_id_nat = as.character(seg_id_nat), date = as.Date(date))

  preds <- left_join(sntemp, ANN) %>%
    left_join(RNN) %>%
    left_join(RGNC) %>%
    left_join(RGNC_ptrn)
  #bring in observations
  obs <- readRDS(sc_retrieve(obs_ind, 'getters.yml')) %>%
    mutate(seg_id_nat = as.character(seg_id_nat))

  pred_obs <- left_join(preds, select(obs, -subseg_id))
  out <- readr:: write_csv(pred_obs, path = out_file)

    return(out)
}



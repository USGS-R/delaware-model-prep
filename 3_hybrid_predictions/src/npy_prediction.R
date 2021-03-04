## creating a function that read data (npy files), process, and combine the data.

munge_npy_dat <- function(in_ind, model_name, seg_vector) {
  # to read npy data files from XJ's predictions (3_hyprid_prediction/in).

  dat_in <- npyLoad(sc_retrieve(in_ind, 'getters.yml'))
   #assign seg_id, we delcared the segs_id in combine_preds_obs funcion.
  dat_in <- as.data.frame(t(dat_in))
  #naming the columns seg_id names.
  names(dat_in) <- seg_vector
  # create date column: assign desired dates (adding a date column in the data).
  dat_in$date <- seq.Date(from = as.Date('2004-10-01'), to = as.Date('2016-09-30'), by = 1)
  # make the data into a long formate.

  dat_mod <- tidyr::gather(dat_in, key = 'seg_id_nat', value = predicted_temp_c , -date)
  # creating a model column.
  dat_mod$model <- model_name
  # return modefied data with date, seg_id_nat, prediction_temp_c, & model columns.
  return(dat_mod)
}

combine_models_obs <- function(obs_ind, ann_ind, rnn_ind, rnn_ptr_ind, rgcn_ind,
                               rgcn_ptrn_ind, rgcn_ptrn_ctr_ind, out_ind){
  # Add to the function arguemnt: ann_npy,

    # Create segment ID vector.
  segs <- c('2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012',
                     '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020',
                     '2021', '2022', '2023', '2024', '2025', '2026', '2027', '2028',
                     '2030', '2031', '2032', '2033', '2034', '2035', '2036', '2037',
                     '2038', '2039', '2040', '2041', '2044', '2045', '2046', '2047',
                     '2048', '4182')
  # 1) plain neural network model.
  ANN <- munge_npy_dat(in_ind = ann_ind, model_name = 'ANN', seg_vector = segs)

  # 2) + time mdoel.
  RNN <- munge_npy_dat(in_ind= rnn_ind, model_name = 'RNN', seg_vector = segs)

  # 3) + time + pretraining
  RNN_ptr <- munge_npy_dat(in_ind= rnn_ptr_ind, model_name = 'RNN_ptrn', seg_vector = segs)

  # 4) + time + space model
  RGCN <- munge_npy_dat(in_ind = rgcn_ind, model_name = 'RGCN', seg_vector = segs)

  # 5) + time + space + pre_training model
  RGCN_ptrn <- munge_npy_dat(in_ind = rgcn_ptrn_ind, model_name = 'RGCN_ptrn', seg_vector = segs)

  # 6) + time + space + pre_training + contrastive loss odel
  RGCN_ptrn_ctr <- munge_npy_dat(in_ind = rgcn_ptrn_ctr_ind, model_name = 'RGCN_ptrn_ctr', seg_vector = segs)

  preds <- bind_rows(ANN, RNN, RNN_ptr, RGCN, RGCN_ptrn, RGCN_ptrn_ctr)
  #bring in observations
  obs_temp_c <- readRDS(sc_retrieve(obs_ind, 'getters.yml')) %>%
    mutate(seg_id_nat = as.character(seg_id_nat))

  pred_obs <- left_join(preds, select(obs_temp_c, -subseg_id))
  out <- saveRDS(pred_obs, file = as_data_file(out_ind))
  gd_put(out_ind)

}

combine_experiments <- function(d100_ind, d02_ind, d001_ind, out_ind) {
  d100 <- readRDS(sc_retrieve(d100_ind, 'getters.yml')) %>%
    mutate(experiment = 'd100')

  d02 <- readRDS(sc_retrieve(d02_ind, 'getters.yml')) %>%
    mutate(experiment = 'd02')

  d001 <- readRDS(sc_retrieve(d001_ind, 'getters.yml')) %>%
    mutate(experiment = 'd001')

  out <- bind_rows(d100, d02, d001)

  saveRDS(out, file = as_data_file(out_ind))
  gd_put(out_ind)

}



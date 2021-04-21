## Creating function to use the metrics function for future input into the Delaware project pipeline:

# Calls the four metrics functions (mae, rmse, mare, and nash). This function accepts 2 arguments data-in and grouping, the inputted data will be grouped by model to suit the data we're working with. The grouping arguments accepts 'NA' for no grouping, or any other grouping such as by segment, month, year, etc. We call the four metrics function to produce a dataframe with metrics of interest.
calc_all_metric <- function(file_path, dat_in, grouping) {
  dat_in <- dat_in %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(model)

  group_test <- deparse(substitute(grouping))
  if (group_test == 'NA') {
    dat_mod <- dat_in
  } else {
    dat_mod <- dat_in %>%
      group_by(model, .data[[grouping]])
  }

  metrics <- dat_mod %>%
    dplyr::summarize(n = n(),
              mae = calc_mae(observe_data = mean_temp_c,
                             predict_data = predicted),
              rmse =  calc_rmse(observe_data = mean_temp_c,
                                predict_data = predicted),
              mare = calc_mare(observe_data = mean_temp_c,
                               predict_data = predicted),
              nse = calc_nash(observe_col = mean_temp_c,
                              predict_col =  predicted))
  write_csv(metrics, path = file_path)
}


# Calls exceedance metric to find when the exceedance of certain temperature was predicted correctly by the models.
calc_exc_metric <- function(file_path, dat_in, grouping) {
  dat_in <- dat_in %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date)) %>%
    group_by(model)

  group_test <- deparse(substitute(grouping))
  if (group_test == 'NA') {
    dat_mod <- dat_in
  } else {
    dat_mod <- dat_in %>%
      group_by(model, .data[[grouping]])
  }
  exceedance_metric <- dat_mod %>%
    dplyr::summarize(n = n(),
            true_pos = calc_exceedance(observe_col = mean_temp_c,
                            predict_col = predicted,
                            metric = 'prop_true_pos',
                            threshold = 25.5),
            true_neg = calc_exceedance(observe_col = mean_temp_c,
                            predict_col = predicted,
                            metric = 'prop_true_neg',
                            threshold = 25.5),
            false_pos = calc_exceedance(observe_col = mean_temp_c,
                            predict_col = predicted,
                            metric = 'prop_false_pos',
                            threshold = 25.5),
            false_neg = calc_exceedance(observe_col = mean_temp_c,
                            predict_col = predicted,
                            metric = 'prop_false_neg'))
  write_csv(exceedance_metric, path = file_path)
}

## Calls the calc_tim_temp_max that returns the max temperature and time of max temps. This function accepts 2 arguments data_in and date-range. date_range in set to summer days. It groups data by the model, segment_id, and year.
calc_max_metric <- function(file_path, dat_in, date_range = 170:245){
  dat_mod <- dat_in %>%
    group_by(model, seg_id_nat, year = lubridate::year(date))

  max_metric <-
    calc_tim_temp_max(data_in = dat_mod,
                    observe_col = mean_temp_c, predict_col = predicted,
                    date_col = date)
  write_csv(max_metric, path = file_path)
}

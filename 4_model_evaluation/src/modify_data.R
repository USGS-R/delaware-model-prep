################ function to read inds file and modify the data to remove all NA's

modify_data <- function(in_ind, ...){
  dat_mod <- feather::read_feather(sc_retrieve(in_ind, remake_file = 'getters.yml')) %>%
    pivot_longer(c(-seg_id_nat, -site_id, -date, -temp_c), names_to = 'model', values_to = 'predicted') %>%
    filter(!is.na(predicted)) %>%
    filter(!is.na(temp_c))
  return(dat_mod)
}



## this function will create subset of the full data and filter for comparison.
procc_data <- function(dat_in, out_file, ...) {
  ## creating a dateframe (4 X 3) with the start and end date for each model .
  dates <- group_by(dat_in, model) %>%
    summarize(start = min(data, end = max(date)))
  start <- max(dates$start)  # finding the max/latest start date.
  end <- min(dates$end)      # finding the min/earliest end date.
  ## filtering the date the and creating a subset with "rgcn_temp_c" model to select distinct seg_id_nat.
  segs <- filter(dat_in, model %in% 'rgcn_temp_c') %>%
    select(seg_id_nat) %>% distinct() %>% pull(seg_id_nat)

  ## Using the evaluate_model functions to calculate the stats for subsets models
  # c4 subsets
  stats_all_models <- filter(dat_in, date >= start & date <= end & seg_id_nat %in% segs) %>%
    calc_all_metric()
    calc_exc_metric()
    calc_max_metric() %>%
    # gsub: taking out the 'temp_c' from model names so its shorter. paste0: adding 'subset' to each model name. mutate" creating a new column called model (overwiting model since it already exists.).
    mutate(model = paste0(gsub('temp_c', '', model, ignore.case = TRUE), 'subset'))
  # Using the evaluate_model functions to calculate the stats for 2 full models
  stats_full_models <- filter(dat_in, date >= start & date <= end) %>%
    calc_all_metric()
    calc_exc_metric()
    calc_max_metric() %>%
    filter(model %in% c('sntemp_temp_c', 'rgcn2_full_temp_c')) %>%
    mutate(model = paste0(gsub('temp_c', '', model, ignore.case = TRUE), 'full'))

  out <- bind_rows(stats_all_models, stats_full_models)
  # write, but don't push to GD
  # going to git commit this one
  write.csv(out, out_file, row.names = FALSE)
}

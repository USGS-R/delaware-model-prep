################ function to read inds file and modify the data to remove all NA's

modify_data <- function(in_ind){
  dat_mod <- feather::read_feather(sc_retrieve(in_ind, remake_file = 'getters.yml')) %>%
    select(-min_temp_c, -max_temp_c) %>%
    pivot_longer(c(-seg_id_nat, -site_id, -date, -mean_temp_c), names_to = 'model', values_to = 'predicted') %>%
    filter(!is.na(predicted)) %>%
    filter(!is.na(mean_temp_c))

  ## creating a dateframe (4 X 3) with the start and end date for each model .
  dates <- group_by(dat_mod, model) %>%
    summarize(start = min(date), end = max(date))
  # finding the max/latest start and min/earliest date
  start <- max(dates$start)
  end <- min(dates$end)
  ## filtering the data and creating a subset with "rgcn_temp_c" model to select distinct seg_id_nat.
  segs <- filter(dat_mod, model %in% 'rgcn_temp_c') %>%
    select(seg_id_nat) %>% distinct() %>% pull(seg_id_nat)


  ## The 4 subsets models: taking a subset of the model
  subset_dat <- filter(dat_mod, date >= start & date <= end & seg_id_nat %in% segs) %>%
    # gsub: taking out the 'temp_c' from model names so its shorter. paste0: adding 'subset' to each model name. mutate" creating a new column called model (overwiting model since it already exists.).
    mutate(model = paste0(gsub('temp_c', '', model, ignore.case = TRUE), 'subset'))
  # Using the evaluate_model functions to calculate the stats for 2 full models
  full_dat <- filter(dat_mod, date >= start & date <= end) %>%
    filter(model %in% c('sntemp_temp_c', 'rgcn2_full_temp_c')) %>%
    mutate(model = paste0(gsub('temp_c', '', model, ignore.case = TRUE), 'full'))

  out <- bind_rows(subset_dat, full_dat)
  # write, but don't push to GD
  # going to git commit this one
  return(out)
}

read_munge_daily_diversions <- function(out_ind) {

  # check if data exists
  # if not, throw error indicating how to download
  if (!file.exists('2_observations/in/reservoir data/2018WY data')) {
    stop("You must download reservoir diversion data from:
  \nhttps://doimspp-my.sharepoint.com/:f:/g/personal/soliver_usgs_gov/EiawwIR59vdApLCkeyob4t0Bx86d2SoTic8ahCm6MH5wRA?e=KEk1zE
  \nand unzip the folder into '2_observations/in'.")
    }

  # point to the data
  # get appropriate filenames
  root_dir <- '2_observations/in/reservoir data'
  all_files <- list.files(root_dir)

  # 2018 and 2019 daily files have some double folders
  # so need to drop 2019WY and 2018WY and keep a2019WY and a2018WY
  sub_dirs <- all_files[!grepl('xls|pdf|csv|duplicates|^2019WY data|^2018WY data', all_files)]

  # handle daily data
  daily_files <- list.files(file.path(root_dir, sub_dirs), full.names = TRUE)
  daily_files_short <- list.files(file.path(root_dir, sub_dirs))

  # map over water years to bind all daily data together
  res_dat <- sub_dirs %>%
    map_df(~map_over_wy_files(sub_dirs = .x, root_dir = root_dir))

  # clean up daily data
  cleaned_res_dat <- res_dat %>%
    mutate(year = stringr::str_extract(string = filename, pattern = '\\d{4}'),
           month = stringr::str_extract(string = filename, pattern = 'January|February|Feburary|March|April|May|June|July|August|September|October|November|December'),
           month = ifelse(month %in% 'Feburary', 'February', month),
           date = paste(year, month, dayofmonth, sep = '-'),
           date = as.Date(date, format = '%Y-%B-%d'),
           doy = lubridate::yday(date))

  # some reservoir/months stored in .csv files that only have one reservoir
  # Pepacton are in .csvs in the folders above
  # Cannonsville are in .csvs in the WY2018 and WY2019 folders

  missing_cannonsville <- c(list.files('2_observations/in/reservoir data/2018WY data'),
                            list.files('2_observations/in/reservoir data/2019WY data'))

  missing_pepacton <- c(list.files('2_observations/in/reservoir data/a2018WY data', full.names = TRUE),
                        list.files('2_observations/in/reservoir data/a2019WY data', full.names = TRUE))
  missing_pepacton <- missing_pepacton[grepl(paste(missing_cannonsville, collapse = '|'), missing_pepacton)]

  # and had to manually add last two missing pepacton files
  missing_pepacton <- c(missing_pepacton,
                        "2_observations/in/reservoir data/a2018WY data/Provisional Data July 2018 (1).csv",
                        "2_observations/in/reservoir data/a2019WY data/Provisional Data November 2018 (1).csv")

  # get full file names for cannonsville
  missing_cannonsville <- c(list.files('2_observations/in/reservoir data/2018WY data', full.names = TRUE),
                            list.files('2_observations/in/reservoir data/2019WY data', full.names = TRUE))

  # map through files, bind data
  my_colnames <- c('dayofmonth', 'precip_in', 'storage_perc',
                   'res_level_ft', 'gross_storage_mg',
                   'storage_change_mgd', 'conservation_release_mgd',
                   'directed_release_mgd', 'total_release_mgd',
                   'spillway_mgd', 'diversion_mgd', 'total_runoff_mgd')

  pepacton_dat <- missing_pepacton %>%
    set_names() %>%
    map_df(~readr::read_csv(file = .x, col_names = my_colnames, skip = 4), .id = 'filename') %>%
    mutate(date = as.Date(dayofmonth, format = '%d-%b-%y'),
           reservoir = 'Pepacton') %>%
    filter(!is.na(date)) %>%
    filter(!is.na(diversion_mgd))

  cannonsville_dat <- missing_cannonsville %>%
    set_names() %>%
    map_df(~readr::read_csv(file = .x, col_names = my_colnames, skip = 4), .id = 'filename') %>%
    mutate(date = as.Date(dayofmonth, format = '%d-%b-%y'),
           reservoir = 'Cannonsville') %>%
    filter(!is.na(date)) %>%
    filter(!is.na(diversion_mgd))

  # write data to csv
  daily_out <- bind_rows(cleaned_res_dat, pepacton_dat, cannonsville_dat) %>%
    select(-dayofmonth, -year, -month, -doy) %>%
    rename(source_file = filename)

  write_csv(x = daily_out, file = as_data_file(out_ind))
  gd_put(out_ind)
}

read_munge_monthly_diversions <- function(in_file, out_ind) {
  # function to read in and munge monthly files that were
  # either manually entered by the NY WSC
  # or by Sam

  # import manually-entered (by us) historical data
  manual_monthly <- read.csv(in_file) %>%
    rename(diversion_cfs = diversion) %>%
    mutate(filename = '2_observations/in/reservoir data/manually_extracted_diversions.csv')

  # import pre-entered (by NY WSC) monthly historical data
  my_colnames <- c('month', 'daysinmonth', 'res_level_ft',
                   'stor_above_sill_mg', 'gross_storage_mg',
                   'storage_change_mgd', 'storage_change_cfs',
                   'diversion_mgd', 'diversion_cfs')

  root_dir <- '2_observations/in/reservoir data/'
  all_files <- list.files(root_dir)

  monthly_files <- all_files[grepl('.xls', all_files)]

  # map over all monthly files, keep track of filename for parsing
  dat <- monthly_files %>%
    set_names() %>%
    purrr::map_df(~readxl::read_excel(file.path(root_dir, .x),
                                      range = 'A12:I24',
                                      col_names = my_colnames), .id = 'filename')

  # pull out identifying info from filename
  dat_cleaned <- dat %>%
    filter(!month %in% 'Calendar Year') %>%
    mutate(wy = as.numeric(gsub('(.*WY)(\\d{4})(.xls)', '\\2', filename)),
           year = ifelse(month %in% c( 'October', 'November', 'December'), wy - 1, wy),
           reservoir = ifelse(grepl('01424997', filename), 'Cannonsville', 'Pepacton')) %>%
    filter(!year %in% 2021)

  # bind monthly data
  out_dat <- bind_rows(dat_cleaned, manual_monthly) %>%
    select(-daysinmonth, -wy)

  write_csv(out_dat, as_data_file(out_ind))
  gd_put(out_ind)
}

interpolate_diversions_to_daily <- function(out_ind, daily_ind, monthly_ind, end_date, mgd_to_cms, cfs_to_cms, mg_to_cm, ft_to_m) {
  daily <- read_csv(sc_retrieve(daily_ind))

  monthly <- read_csv(sc_retrieve(monthly_ind)) %>%
    rename(source_file = filename) %>%
    mutate(diversion_cms = diversion_cfs*cfs_to_cms) %>%
    select(-storage_change_cfs, -diversion_cfs, -diversion_mgd)

  # create a dataframe with all dates in timeseries for each reservoir
  all_dates <- data.frame(date = seq(from = as.Date('1979-10-01'),
                                          to = as.Date(end_date), by = 1)) %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date, label = TRUE, abbr = FALSE))

  all_dates <- bind_rows(mutate(all_dates, reservoir = 'Pepacton'), mutate(all_dates, reservoir = 'Cannonsville'))

  # find the missing reservoir-date combos From daily data
  # and merge monthly data by year, month, and reservoir to gap fill
  # this repeats monthly values to each day in that month
  monthly$month <- factor(as.factor(monthly$month), levels = levels(all_dates$month), ordered = TRUE)
  missing_dailies <- left_join(all_dates, daily) %>%
    filter(is.na(diversion_mgd)) %>%
    select(date, year, month, reservoir) %>%
    left_join(monthly) %>%
    mutate(data_type = 'monthly')

  # combine daily and monthly
  # convert units
  # clean up column and row ordering
  all_dat <- daily %>%
    mutate(data_type = 'daily',
           diversion_cms = diversion_mgd*mgd_to_cms) %>%
    bind_rows(missing_dailies) %>%
    mutate(res_level_m = res_level_ft*ft_to_m,
           gross_storage_m3 = gross_storage_mg*mg_to_cm,
           storage_change_cms = storage_change_mgd*mgd_to_cms,
           conservation_release_cms = conservation_release_mgd*mgd_to_cms,
           directed_release_cms = directed_release_mgd*mgd_to_cms,
           total_release_cms = total_release_mgd*mgd_to_cms,
           spillway_cms = spillway_mgd*mgd_to_cms,
           total_runoff_cms = total_runoff_mgd*mgd_to_cms,
           storage_above_sill_m3 = stor_above_sill_mg*mg_to_cm) %>%
    arrange(reservoir, date) %>%
    select(-year, -month, -contains('_mg'), -res_level_ft) %>%
    select(reservoir, date, diversion_cms, everything()) %>%
    relocate(source_file, .after = last_col())

  write_csv(all_dat, file = as_data_file(out_ind))
  gd_put(out_ind)
}

#########################################
# Functions used to map over daily files
#########################################

# map over sheets within a single monthly file
# pulling out only Pepacton and Cannsonville, but more included in spreadsheeds
# however, there were some inconsistencies in other Reservoirs, so wanted to
# limit the edge cases I needed to handle now
read_and_munge_diversions <- function(location, in_file) {
  full_path <- file.path(location, in_file)

  # print message to give indication of what file we're on
  message(paste('Importing data from', in_file))
  my_colnames <- c('dayofmonth', 'precip_in', 'storage_perc',
                   'res_level_ft', 'gross_storage_mg',
                   'storage_change_mgd', 'conservation_release_mgd',
                   'directed_release_mgd', 'total_release_mgd',
                   'spillway_mgd', 'diversion_mgd', 'total_runoff_mgd')
  dat <- full_path %>%
    excel_sheets() %>%
    .[grepl('cannon|pepac', ., ignore.case = TRUE)] %>%
    set_names() %>%
    purrr::map_df(~readxl::read_excel(full_path, sheet = .x,
                                      range = 'A5:L43',
                                      col_names = my_colnames), .id = 'reservoir') %>%
    mutate(filename = in_file)

  return(dat)

}

# go through each water year to read in and bind
# each monthly file
map_over_wy_files <- function(sub_dirs, root_dir) {

  # print message to give indication of what folder we're on
  message(paste('Importing data from', sub_dirs))
  # create file path in new sub director
  wy_path <- file.path(root_dir, sub_dirs)
  wy_files <- list.files(wy_path)
  wy_files <- wy_files[!grepl('csv', wy_files, ignore.case = TRUE)]

  if(!length(wy_files) == 12) {
    warning(paste(sub_dirs, ' does not contain 12 monthly files'))
  }

  dat2 <- wy_files %>%
    map_df(~read_and_munge_diversions(location = wy_path, in_file = .x)) %>%
    filter(dayofmonth %in% 1:31) %>%
    filter(!is.na(precip_in))

  return(dat2)
}

combine_release_sources <- function(out_ind, hist_rel_ind, usgs_rel_ind, modern_rel_ind, manual_rel_ind) {

    hist <- readr::read_csv(sc_retrieve(hist_rel_ind, 'getters.yml'))
    usgs <- readr::read_csv(sc_retrieve(usgs_rel_ind, 'getters.yml'))
    modern <- readRDS(sc_retrieve(modern_rel_ind, 'getters.yml'))
    manual <- readr::read_csv(sc_retrieve(manual_rel_ind, 'getters.yml'))

    # bind together
    hist_out <- group_by(hist, date, reservoir) %>%
      summarize(release_volume_cms = sum(release_volume_cms)) %>%
      ungroup()
    usgs_out <- usgs %>%
      select(reservoir, date, total_release_mgd) %>%
      mutate(release_volume_cms = total_release_mgd*0.0438125) %>%
      select(-total_release_mgd)
    manual_out <- manual %>%
      pivot_longer(cols = -Date, names_to = 'reservoir', values_to = 'release_volume_cfs') %>%
      mutate(release_volume_cms = release_volume_cfs/35.314666) %>%
      mutate(date = as.Date(Date, format = '%m/%d/%Y')) %>%
      select(-release_volume_cfs, -Date) %>%
      filter(!is.na(release_volume_cms))

    modern_out <- select(modern, reservoir, date, release_volume_cms = total_releases_cms)

    # group by and slice_min picks the sources in order of the bind
    # e.g., always use the historical data from the NYC DEP, and then prioritize
    # data in the order of modern pull from NWIS, manually data from ODRM,
    # and data imported from the NY WSC (NY WSC data was messies/most prone to
    # error in importing)
    all <- bind_rows(hist_out, modern_out, manual_out, usgs_out, .id = 'id') %>%
      group_by(reservoir, date) %>%
      slice_min(id) %>%
      select(-id) %>%
      mutate(release_volume_cms = round(release_volume_cms, 3))

    readr::write_csv(all, as_data_file(out_ind))
    gd_put(out_ind)


}

combine_releases_by_type <- function(out_ind, hist_rel_ind, modern_rel_ind) {
  hist <- readr::read_csv(sc_retrieve(hist_rel_ind, 'getters.yml'))
  hist_spills <- filter(hist, release_type %in% 'Spill') %>%
    mutate(release_type = 'spillway')
  hist_releases <- filter(hist, !release_type %in% 'Spill') %>%
    group_by(date, reservoir, GRAND_ID) %>%
    summarize(release_volume_cms = sum(release_volume_cms)) %>%
    mutate(release_type = 'releases') %>% ungroup()
  mod <- readRDS(sc_retrieve(modern_rel_ind, 'getters.yml')) %>%
    select(date, reservoir, releases_cms, spillway_cms) %>%
    tidyr::pivot_longer(cols = c(releases_cms, spillway_cms), names_to = 'release_type', values_to = 'release_volume_cms') %>%
    mutate(release_type = gsub('_cms', '', release_type))

  out <- bind_rows(hist_spills, hist_releases, filter(mod, !date %in% unique(hist$date))) %>%
    select(-GRAND_ID)

  readr::write_csv(out, as_data_file(out_ind))
  gd_put(out_ind)
}

extract_diversions <- function(out_ind, hist_ind, mod_ind) {

  hist_dat <- readr::read_csv(sc_retrieve(hist_ind))

  # use historical data when we have it since that is the data
  # source for most of the record
  mod_dat <- readRDS(sc_retrieve(mod_ind)) %>%
    filter(date > max(hist_dat$date))

  out <- hist_dat %>%
    select(reservoir, date, diversion_cms) %>%
    bind_rows(select(mod_dat, reservoir, date, diversion_cms = diversions_cms)) %>%
    filter(reservoir %in% c('Cannonsville', 'Pepacton'))

  readr::write_csv(out, as_data_file(out_ind))
  gd_put(out_ind)
}





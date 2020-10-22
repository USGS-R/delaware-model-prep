
#'run SNTemp model executable
#'
#'@param start start date for SNTemp model
#'@param stop stop date for SNTemp model
#'@param spinup T/F for whether or not to run a spinup period (this is necessary for most data assimilation appications)
#'@param spinup_days number of days of the spinup period. The spinup period will be from the start date - spinup_days to start date - 1
#'@param restart T/F for whether or not to initialize SNTemp based on end of previous run
#'@param control_file name of the control file to set variables
run_sntemp = function(out_ind = NULL,
                      start,
                      stop,
                      spinup = F,
                      spinup_days = 730,
                      restart = F,
                      model_run_loc,
                      control_file = 'delaware.control',
                      precip_file = './input/prcp.cbh',
                      tmax_file = './input/tmax.cbh',
                      tmin_file = './input/tmin.cbh',
                      sf_data_file = './input/sf_data',
                      var_init_file = 'prms_ic.txt',
                      var_save_file = 'prms_ic.txt'){

  if(spinup){
    print('Running spinup period...')
    run_sntemp_spinup(spinup_days = spinup_days,
                      start = start,
                      control_file = control_file,
                      model_run_loc = model_run_loc,
                      precip_file = precip_file,
                      tmax_file = tmax_file,
                      tmin_file = tmin_file,
                      sf_data_file = sf_data_file,
                      var_init_file = var_init_file,
                      var_save_file = var_save_file)
  }
  if(restart){
    ctrl = readLines(file.path(model_run_loc, 'control', control_file)) # read in control file

    init_vars_loc = grep('init_vars_from_file', ctrl) + 3
    save_vars_loc = grep('save_vars_to_file', ctrl) + 3
    ctrl[init_vars_loc] = '1'
    ctrl[save_vars_loc] = '1'

    precip_file_loc = grep('precip_day', ctrl) + 3
    tmax_file_loc = grep('tmax_day', ctrl) + 3
    tmin_file_loc = grep('tmin_day', ctrl) + 3
    sf_data_file_loc = grep('data_file', ctrl) + 3
    ctrl[precip_file_loc] = precip_file
    ctrl[tmax_file_loc] = tmax_file
    ctrl[tmin_file_loc] = tmin_file
    ctrl[sf_data_file_loc] = sf_data_file

    var_init_file_loc = grep('var_init_file', ctrl) + 3
    var_save_file_loc = grep('var_save_file', ctrl) + 3
    ctrl[var_init_file_loc] = var_init_file
    ctrl[var_save_file_loc] = var_save_file

    writeLines(text = ctrl, con = file.path(model_run_loc, 'control', control_file))
  }else{
    ctrl = readLines(file.path(model_run_loc, 'control', control_file)) # read in control file

    init_vars_loc = grep('init_vars_from_file', ctrl) + 3
    save_vars_loc = grep('save_vars_to_file', ctrl) + 3
    ctrl[init_vars_loc] = '0'
    ctrl[save_vars_loc] = '1'

    precip_file_loc = grep('precip_day', ctrl) + 3
    tmax_file_loc = grep('tmax_day', ctrl) + 3
    tmin_file_loc = grep('tmin_day', ctrl) + 3
    sf_data_file_loc = grep('data_file', ctrl) + 3
    ctrl[precip_file_loc] = precip_file
    ctrl[tmax_file_loc] = tmax_file
    ctrl[tmin_file_loc] = tmin_file
    ctrl[sf_data_file_loc] = sf_data_file

    var_init_file_loc = grep('var_init_file', ctrl) + 3
    var_save_file_loc = grep('var_save_file', ctrl) + 3
    ctrl[var_init_file_loc] = var_init_file
    ctrl[var_save_file_loc] = var_save_file

    writeLines(text = ctrl, con = file.path(model_run_loc, 'control', control_file))
  }

  set_sntemp_start_stop(start = start , stop = stop, model_run_loc = model_run_loc, control_file = control_file)

  current.wd = getwd() # getting current project root wd to reset after running batch file

  setwd(file.path(current.wd, model_run_loc)) # set wd to where batch file lives
  shell('delaware.bat') # run batch file

  setwd(current.wd) # set wd back to root of project

  if(!is.null(out_ind)){ # write .ind file if supplied
    sc_indicate(ind_file = out_ind)
  }
}

run_sntemp_spinup = function(spinup_days, start,
                             model_run_loc,
                             control_file,
                             precip_file,
                             tmax_file,
                             tmin_file,
                             sf_data_file,
                             var_init_file,
                             var_save_file){

  # how many days before start date. end date should be one day before start date
  spinup_start = as.Date(as.character(start)) - spinup_days - 1
  spinup_stop = as.Date(as.character(start)) - 1

  ctrl = readLines(file.path(model_run_loc, 'control', control_file)) # read in control file

  init_vars_loc = grep('init_vars_from_file', ctrl) + 3
  save_vars_loc = grep('save_vars_to_file', ctrl) + 3

  start_year_loc = grep('start_time', ctrl) + 3
  start_month_loc = grep('start_time', ctrl) + 4
  start_day_loc = grep('start_time', ctrl) + 5

  stop_year_loc = grep('end_time', ctrl) + 3
  stop_month_loc = grep('end_time', ctrl) + 4
  stop_day_loc = grep('end_time', ctrl) + 5

  # update init_vars so that it doesn't read initial conditions file since this is spinup period for producing an initial condition file
  ctrl[init_vars_loc] = '0'
  ctrl[save_vars_loc] = '1'

  # update start / stop
  ctrl[start_year_loc] = as.character(lubridate::year(spinup_start))
  ctrl[start_month_loc] = as.character(lubridate::month(spinup_start))
  ctrl[start_day_loc] = as.character(lubridate::day(spinup_start))

  ctrl[stop_year_loc] = as.character(lubridate::year(spinup_stop))
  ctrl[stop_month_loc] = as.character(lubridate::month(spinup_stop))
  ctrl[stop_day_loc] = as.character(lubridate::day(spinup_stop))

  precip_file_loc = grep('precip_day', ctrl) + 3
  tmax_file_loc = grep('tmax_day', ctrl) + 3
  tmin_file_loc = grep('tmin_day', ctrl) + 3
  sf_data_file_loc = grep('data_file', ctrl) + 3
  ctrl[precip_file_loc] = precip_file
  ctrl[tmax_file_loc] = tmax_file
  ctrl[tmin_file_loc] = tmin_file
  ctrl[sf_data_file_loc] = sf_data_file

  var_init_file_loc = grep('var_init_file', ctrl) + 3
  var_save_file_loc = grep('var_save_file', ctrl) + 3
  ctrl[var_init_file_loc] = var_init_file
  ctrl[var_save_file_loc] = var_save_file

  writeLines(text = ctrl, con = file.path(model_run_loc, 'control', control_file))

  current.wd = getwd() # getting current project root wd to reset after running batch file

  setwd(file.path(current.wd, model_run_loc)) # set wd to where batch file lives
  shell('delaware.bat') # run batch file

  setwd(current.wd) # set wd back to root of project
}



#'
#'@param start start date of the model run
#'@param stop stop date of the model run
#'@param control_file name of the control file
#'
set_sntemp_start_stop = function(start, stop,
                                 model_run_loc,
                                 control_file){

  start = as.Date(as.character(start))
  stop = as.Date(as.character(stop))

  ctrl = readLines(file.path(model_run_loc, 'control', control_file)) # read in control file

  start_year_loc = grep('start_time', ctrl) + 3
  start_month_loc = grep('start_time', ctrl) + 4
  start_day_loc = grep('start_time', ctrl) + 5

  stop_year_loc = grep('end_time', ctrl) + 3
  stop_month_loc = grep('end_time', ctrl) + 4
  stop_day_loc = grep('end_time', ctrl) + 5

  # update start / stop
  ctrl[start_year_loc] = as.character(lubridate::year(start))
  ctrl[start_month_loc] = as.character(lubridate::month(start))
  ctrl[start_day_loc] = as.character(lubridate::day(start))

  ctrl[stop_year_loc] = as.character(lubridate::year(stop))
  ctrl[stop_month_loc] = as.character(lubridate::month(stop))
  ctrl[stop_day_loc] = as.character(lubridate::day(stop))

  writeLines(text = ctrl, con = file.path(model_run_loc, 'control', control_file))
}



#' For hydroPSO functions
#'
set_sntemp_restart = function(restart,
                              model_run_loc,
                              control_file,
                              var_init_file,
                              var_save_file){

  if(restart){
    ctrl = readLines(file.path(model_run_loc, 'control', control_file)) # read in control file

    init_vars_loc = grep('init_vars_from_file', ctrl) + 3
    save_vars_loc = grep('save_vars_to_file', ctrl) + 3
    ctrl[init_vars_loc] = '1'
    ctrl[save_vars_loc] = '1'

    var_init_file_loc = grep('var_init_file', ctrl) + 3
    var_save_file_loc = grep('var_save_file', ctrl) + 3
    ctrl[var_init_file_loc] = var_init_file
    ctrl[var_save_file_loc] = var_save_file

    writeLines(text = ctrl, con = file.path(model_run_loc, 'control', control_file))
  }else{
    ctrl = readLines(file.path(model_run_loc, 'control', control_file)) # read in control file

    init_vars_loc = grep('init_vars_from_file', ctrl) + 3
    save_vars_loc = grep('save_vars_to_file', ctrl) + 3
    ctrl[init_vars_loc] = '0'
    ctrl[save_vars_loc] = '1'

    var_init_file_loc = grep('var_init_file', ctrl) + 3
    var_save_file_loc = grep('var_save_file', ctrl) + 3
    ctrl[var_init_file_loc] = var_init_file
    ctrl[var_save_file_loc] = var_save_file

    writeLines(text = ctrl, con = file.path(model_run_loc, 'control', control_file))
  }
}


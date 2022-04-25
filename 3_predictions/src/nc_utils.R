
#' Wrapper function that creates nc file, gathers PRMS-SNTemp output, and stores output in nc file
#'
#' @param nc_name_out name of the netcdf file to be created and store PRMS-SNTemp output
#' @param model_run_loc location of the PRMS-SNTemp model run
#' @param model_output_file file path to the PRMS-SNTemp variable output
#' @param model_locations_file file path to the rds file that has PRMS model idx and seg_id_nat
#' @param dynamic_vars vector of variables we want to store from PRMS-SNTemp model run
#' @param static_vars vector of variables we want to store that describe segment characteristics (i.e. static)
#' @param vars_desc description of the variables which will be used to describe the netcdf file
#' @param n the ensemble member to be stored
#' @param n_en total number of ensemble members
#' @param dates vector of dates of the model run
#' @param project_id string of the model project id
#'
nc_store_output <- function(nc_name_out,
                            model_run_loc,
                            model_output_file,
                            model_locations_file,
                            dynamic_vars,
                            static_vars,
                            vars_desc,
                            n,
                            n_en,
                            dates,
                            project_id){

  vars = c(dynamic_vars, static_vars)

  # might have to move this out of this function if we want to store multiple PRMS-SNTemp ensembles in one nc file
  nc_create_pb_out(model_locations_file,
                   n_en,
                   dates,
                   project_id,
                   vars,
                   vars_desc,
                   nc_name_out)

  # gather prms-sntemp output
  model_out = gather_sntemp_output(model_run_loc = model_run_loc,
                                   model_output_file = model_output_file,
                                   model_fabric_file = 'GIS/Segments_subset.shp',
                                   sntemp_vars = dynamic_vars)

  print(sprintf('storing prms-sntemp %s of %s ensembles in ncdf', n, n_en))

  # put model output into nc file
  nc_model_put(var_df = model_out,
               var_names = vars,
               ens = n,
               nc_file = nc_name_out)

  return(nc_name_out)
}


#' Creates a NetCDF file for storing PRMS-SNTemp output
#'
#' @param model_locations_file rds file with the locations of the PRMS-SNTemp output variables
#' @param n_en number of ensemble runs of PRMS-SNTemp
#' @param dates vector of model dates
#' @param project_id string of the model project id
#' @param vars vector of variables we want to store from PRMS-SNTemp model run
#' @param vars_desc description of the variables which will be used to describe the netcdf file
#' @param nc_name_out netcdf file name output
#' @param overwrite T/F if the nc file should be overwritten if it exists already
#'
nc_create_pb_out = function(model_locations_file,
                            n_en,
                            dates,
                            project_id,
                            vars,
                            vars_desc,
                            nc_name_out,
                            overwrite = T){

  #Set dimensions; should always be [(forecast_issue_time), time, (vertical dimension), location (or lon/ lat), ens];
  #  for this project, we should have the dimensions of [time, model_location, ens]
  n_times <- length(dates)
  times <- as.integer(seq(0, n_times - 1, 1)) # days since dates[1]
  model_locations <- as.integer(readRDS(model_locations_file)$seg_id_nat) # PRMS-SNTemp seg_id_nat
  ens <- as.integer(seq(1, n_en, 1))

  time_dim <- ncdim_def("time",
                        units = sprintf('days since %s', dates[1]),
                        longname = 'time',
                        vals = times)
  loc_dim <- ncdim_def("seg_id_nat",
                       units = "",
                       vals = model_locations,
                       longname = 'PRMS-SNTemp stream segment id national')
  ens_dim <- ncdim_def("ens",
                       units = "",
                       vals = ens,
                       longname = 'ensemble member')

  dim_nchar <- ncdim_def("nchar",
                         units = "",
                         vals = 1:nchar(as.character(dates[1])),
                         create_dimvar = FALSE)
  ## quick check that units are valid
  udunits2::ud.is.parseable(ens_dim$units)
  udunits2::ud.is.parseable(loc_dim$units)
  udunits2::ud.is.parseable(time_dim$units)
  udunits2::ud.is.parseable(dim_nchar$units)

  #Define variables
  fillvalue <- 1e32

  def_list <- list()
  # loop through variables that we're predicting
  n_vars = length(vars)

  for(i in 1:n_vars){
    cur_var = vars[i]

    def_list[[i]] <- ncvar_def(name =  vars_desc$state[vars_desc$state==cur_var],
                               units = vars_desc$units[vars_desc$state==cur_var],
                               dim = list(time_dim, loc_dim, ens_dim),
                               missval = fillvalue,
                               longname = vars_desc$longname[vars_desc$state==cur_var],
                               prec = vars_desc$prec[vars_desc$state==cur_var])
  }


  if(file.exists(nc_name_out)){
    if(overwrite){
      file.remove(nc_name_out)
      ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
    }else{stop('cannot overwrite nc output file')}
  }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}


  #Global file metadata
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "project_id",
            attval = as.character(project_id),
            prec =  "text")

  nc_close(ncout)
  return(nc_name_out)
}



#' insert PRMS-SNTemp output into NetCDF file
#'
#' @param var_df data frame of the PRMS-SNTemp model output, will be generated from gather_sntemp_output()
#' @param var_names vector of variable names to store from PRMS-SNTemp
#' @param ens current ensemble member being stored
#' @param nc_file file path to the netcdf file
#'
nc_model_put = function(var_df,
                        var_names,
                        ens, # current ensemble
                        nc_file){

  ncout <- nc_open(nc_file, write = T)

  all_seg_id_nats = as.numeric(unique(var_df$seg_id_nat))

  for(i in seq_along(var_names)){
    print(sprintf('   storing %s', var_names[i]))
    cur_var = ncout$var[[var_names[i]]]
    varsize = cur_var$varsize
    # temp output dims [time_dim, loc_dim, ens_dim]; position of dimensions following:
    time_pos = 1
    loc_pos = 2
    ens_pos = 3

    n_dims = cur_var$ndims

    nc_seg_id_nats = ncout$dim$seg_id_nat$vals

    for(j in seq_along(nc_seg_id_nats)){
      cur_seg_id_nat = which(nc_seg_id_nats[j] == nc_seg_id_nats)
      cur_en = ens

      n_en = varsize[ens_pos]

      start = rep(1, n_dims)
      start[ens_pos] = cur_en
      start[loc_pos] = cur_seg_id_nat

      count = varsize
      count[ens_pos] = 1 # adding output from only one ensemble
      count[loc_pos] = 1 # adding output from only one location

      cur_output = dplyr::filter(var_df, as.numeric(seg_id_nat) == nc_seg_id_nats[j]) %>%
        dplyr::select(var_names[i]) %>% pull()

      # qa / qc values


      ncvar_put(nc = ncout,
                varid = var_names[i],
                vals = cur_output,
                start = start,
                count = count)
    }
  }

  nc_close(ncout)
}


#' function for returning PRMS-SNTemp variables; returns tibble
#'
#' @param nc_file file path to the netcdf file
#' @param var_name PRMS-SNTemp output variable name you want returned from the netcdf file
#' @param times model times you want returned
#' @param seg_id_nats seg_id_nat segments you want returned
#' @param ens ensembles members you want returned
#'
nc_model_get = function(nc_file,
                        var_name,
                        times = NULL,
                        seg_id_nats = NULL,
                        ens = NULL){

  ncout = nc_open(nc_file)

  # output dims [time_dim, loc_dim, ens_dim]; position of dimensions following:
  time_pos = 1
  loc_pos = 2
  ens_pos = 3

  cur_var = ncout$var[[var_name]]
  varsize = cur_var$varsize
  all_valid_times = ncdf_times(nc = ncout)
  all_seg_id_nats = as.character(cur_var$dim[[loc_pos]]$vals)
  all_ens = as.integer(cur_var$dim[[ens_pos]]$vals)

  n_dims = cur_var$ndims

  # return all values, and then filter
  all_out = array(ncvar_get(nc = ncout, varid = var_name), dim = varsize) %>%
    reshape2::melt(varnames = c('time', 'seg_id_nat', 'ensemble')) %>%
    mutate(time = all_valid_times[time],
           seg_id_nat = all_seg_id_nats[seg_id_nat],
           ensemble = all_ens[ensemble]) %>%
    rename(!!var_name := value) %>%
    as_tibble()

  if(!is.null(times)){
    cur_valid_times = as.Date(times)
  }else{cur_valid_times = as.Date(all_valid_times)} # return all time if NULL
  if(!is.null(seg_id_nats)){
    cur_seg_id_nats = as.character(seg_id_nats)
  }else{cur_seg_id_nats = as.character(all_seg_id_nats)} # return all seg_id_nats if NULL
  if(!is.null(ens)){
    cur_ens = as.integer(ens)
  }else{cur_ens = as.integer(all_ens)}

  out = dplyr::filter(all_out,
                      time %in% cur_valid_times,
                      seg_id_nat %in% cur_seg_id_nats,
                      ensemble %in% cur_ens)

  nc_close(ncout)

  return(out)
}




#' Get time attribute from NetCDF file and convert it to years (or Rdate)
#'
#' This function reads the NetCDF time attribute and converts it to years
#' with decimal fraction for monthly data. For sub-monthly data the conversion
#' is to the Rdate format (only works with standard calendar!).
#'
#' original code from https://rdrr.io/github/jonasbhend/geoutils/src/R/ncdf_times.R
#'
#' @param nc object with link to NetCDF file (from \code{\link{open.ncdf}})
#' @param timename dimension name with time (optional)
#' @param as.Rdate logical, should output be converted to Rdate?
#' @param force logical, force Rdate conversion for monthly times?
#' @param tz time zone for time zone support (experimental)
#'
ncdf_times <- function(nc, timename=NULL, as.Rdate=TRUE, force=TRUE, tz="UTC") {
  ## this function converts netcdf times to the
  ## R date-time format or to the udunits dates
  ## you can choose to switch to uduints format
  ## for gregorian calendars by setting as.Rdate
  ## to FALSE
  ## non-gregorian calendar dates are output using
  ## udunits date format

  ## you can force to get back an R date format, even
  ## if the calendar used is not gregorian using
  ## force=T (may return udunits dates if conversion
  ## is not successful)

  ## WARNING: time zones are not fully supported yet

  ## check whether udunits is available
  .udunitsInclude     <- FALSE
  if (any(.packages() == "udunits") & class(try(utInit(), silent=T)) != "try-error"){
    .udunitsInclude <- TRUE
  }
  if (is.null(timename)){
    timei   <- which(names(nc$dim) %in% c("time", "TIME", "tim", "TIM"))
  } else {
    timei <- which(names(nc$dim) == timename)
  }
  units   <- nc$dim[[timei]]$units
  refdate <- strsplit(units, " ")[[1]]
  refdate <- refdate[grep('-', refdate)]
  ## debug reference date
  if (substr(refdate, nchar(refdate) - 1, nchar(refdate)) == '00') {
    rtmp <- strsplit(refdate, '-')[[1]]
    refdate <- paste(c(rtmp[-length(rtmp)], '01'), collapse='-')
    rm(rtmp)
  }
  vals    <- nc$dim[[timei]]$vals
  tmp     <- ncatt_get(nc, names(nc$dim)[timei], "calendar")
  if (tmp$hasatt) {
    calendar <- tmp$value
  } else {
    calendar <- "standard"
    ## print(paste("Warning: Calendar is missing in", nc$filename))
  }
  if (calendar == "proleptic_gregorian" || calendar == "gregorian") calendar <- "standard"

  if (as.Rdate){

    if (charmatch("hours since", units, nomatch=0) |
        charmatch("minutes since", units, nomatch=0) |
        charmatch("seconds since", units, nomatch=0) & calendar == 'standard') {

      mul <- 1
      ref.txt     <- substr(units, 15,33)
      if (charmatch("minutes", units, nomatch=0)) mul     <- 60
      if (charmatch("hours", units, nomatch=0)) {
        mul     <- 3600
        ref.txt <- substr(units, 13,31)
      }

      times       <- vals * mul
      if (nchar(ref.txt) == 19){
        ref         <- as.POSIXct(ref.txt, tz)
      } else {
        ref         <- as.POSIXct(paste(ref.txt, "00", sep=":"), tz)
      }
      time        <- as.Date(ref + times)

    } else if (charmatch("days since", units, nomatch=0) & calendar == 'standard'){
      time        <- as.Date(refdate, "%Y-%m-%d") + vals
    } else if (charmatch("days since", units, nomatch=0) &
               calendar %in% c('365_day', 'noleap', '360_day')) {
      if (calendar == '365_day' || calendar == 'noleap'){
        vals <- round(vals/365*365.24*2)/2
        time <- as.Date(refdate, "%Y-%m-%d") + vals
      } else if (calendar == '360_day'){
        vals <- round(vals/360*365.24*2)/2
        time <- as.Date(refdate, "%Y-%m-%d") + vals
      }
    } else if (charmatch("months since", units, nomatch=0)) {
      ref.yr      <- as.numeric(format(as.Date(refdate), '%Y'))
      ref.month   <- as.numeric(format(as.Date(refdate), '%m'))
      ref.day     <- as.numeric(format(as.Date(refdate), '%d'))
      if (is.null(ref.day) | ref.day == 0) ref.day <- 1

      month       <- floor((vals+ref.yr*12 + ref.month-1) %% 12) + 1
      year        <- floor((vals+ref.yr*12 + ref.month-1)/12)

      time        <- as.Date(ISOdate(year, month, ref.day))
    } else if (charmatch("years since", units, nomatch=0)) {
      unit.tmp    <- paste(strsplit(units, " ")[[1]][3:4], collapse=" ")
      ## ref.yr      <- substr(units, 13,16)
      ## ref.month   <- as.numeric(substr(units, 18,19))
      ref.yr      <- as.numeric(format(as.Date(unit.tmp), "%Y"))
      ref.month   <- as.numeric(format(as.Date(unit.tmp), "%m"))
      if (is.null(ref.month)) ref.month <- 1
      ##ref.day     <- as.numeric(substr(units, 21,22))
      ref.day     <- as.numeric(format(as.Date(unit.tmp), "%d"))
      if (is.null(ref.day)) ref.day <- 1

      year        <- floor(vals)
      month       <- floor((vals*12)%%12)

      time        <- as.Date(ISOdate(ref.yr + year, ref.month + month, ref.day))
    }  else if (charmatch("day as", units, nomatch=0)) {
      date        <- floor(vals)
      day         <- as.numeric(substr(date, nchar(date)-1, nchar(date)))
      if (all(day > 28)) date <- as.character(as.numeric(date) - max(day, na.rm=T) + 28)
      date        <- paste("000",date, sep="")
      date        <- substr(date, nchar(date)-7, nchar(date))
      time        <- as.Date(date, "%Y%m%d")
    } else {
      stop(paste("Can't deal with calendar", calendar))
    }

  } else {
    if (.udunitsInclude){
      time <- utCalendar(vals, units, calendar=calendar, style="array")
      if (force){
        tmp  <- try(ISOdatetime(time$year, time$month, time$day, time$hour,
                                time$minute, time$second, tz), silent=T)
        if (class(tmp)[1] != "try-error") time <- tmp
      }
    } else {
      warning("Package udunits cannot be loaded or initialized via utInit()")
    }
  }


  return(time)

}


# create nc file for storing PRMS-SNTemp parameters
#' nc_create_pb_params = function(model_locations_ind,
#'                                n_en,
#'                                project_id,
#'                                vars, # parameter names
#'                                nc_name_out_ind,
#'                                model_run_loc,
#'                                param_default_file = 'control/delaware.control.par_name',
#'                                n_segments = 456,
#'                                n_hrus = 765,
#'                                n_gwr = 765,
#'                                n_ssr = 765,
#'                                overwrite = T,
#'                                gd_config = 'lib/cfg/gd_config.yml'){
#'
#'   model_locations <- as.integer(readRDS(sc_retrieve(model_locations_ind, remake_file = 'getters.yml'))$seg_id_nat) # PRMS-SNTemp seg_id_nat
#'
#'   #Set dimensions
#'   ens <- as.integer(seq(1, n_en, 1))
#'   seg_model_idxs <- as.integer(seq(1, n_segments, 1))
#'   hru_model_idxs <- as.integer(seq(1, n_hrus, 1))
#'   gwr_model_idxs <- as.integer(seq(1, n_gwr, 1))
#'   ssr_model_idxs <- as.integer(seq(1, n_ssr, 1))
#'
#'   ens_dim <- ncdim_def("ens",
#'                        units = "",
#'                        vals = ens,
#'                        longname = 'ensemble member')
#'   seg_dim <- ncdim_def("seg_loc",
#'                        units = "",
#'                        vals = seg_model_idxs,
#'                        longname = 'stream segment model index')
#'   hru_dim <- ncdim_def("hru_loc",
#'                        units = "",
#'                        vals = hru_model_idxs,
#'                        longname = 'HRU model index')
#'   gwr_dim <- ncdim_def("gwr_loc",
#'                        units = "",
#'                        vals = gwr_model_idxs,
#'                        longname = 'groundwater reservoir model index')
#'   ssr_dim <- ncdim_def("ssr_loc",
#'                        units = "",
#'                        vals = ssr_model_idxs,
#'                        longname = 'shallow subsurface reservoir model index')
#'   one_dim <- ncdim_def('global_loc',
#'                        units = '',
#'                        vals = 1,
#'                        longname = 'global parameter; applied to every segment or hru')
#'   month_dim <- ncdim_def('month_param',
#'                          units = '',
#'                          vals = seq(1,12,1),
#'                          longname = 'parameter applied either globally by segment / hru for a given month')
#'
#'   #Define variables
#'   fillvalue_float <- 1e32
#'   fillvalue_int <- -99
#'
#'   # use same dimensions as NOAA forecasts [lon, lat, forecast hours, ensemble, issue date]
#'   def_list <- list()
#'   # loop through variables that we're forecasting
#'   n_vars = length(vars$param)
#'   for(i in 1:n_vars){
#'     cur_defaults = get_default_param_vals(param_name = vars$param[i],
#'                                           model_run_loc = model_run_loc,
#'                                           param_default_file = param_default_file)
#'     cur_time_dim = NULL
#'
#'     if(cur_defaults$dim == 'one'){
#'       cur_loc_dim = one_dim
#'     }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
#'       cur_loc_dim = seg_dim
#'     }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
#'       cur_loc_dim = hru_dim
#'     }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
#'       cur_loc_dim = gwr_dim
#'     }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
#'       cur_loc_dim = ssr_dim
#'     }else if(cur_defaults$dim == 'nmonths'){
#'       cur_loc_dim = one_dim
#'       cur_time_dim = month_dim
#'     }else if(cur_defaults$ndim == '2'){
#'       if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
#'         cur_loc_dim = seg_dim
#'         cur_time_dim = month_dim
#'       }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
#'         cur_loc_dim = hru_dim
#'         cur_time_dim = month_dim
#'       }
#'     }
#'     # print(cur_loc_dim$name)
#'
#'     if(!is.null(cur_time_dim)){
#'       def_list[[i]] <- ncvar_def(name =  vars$param[i],
#'                                  units = vars$units[i],
#'                                  dim = list(cur_loc_dim, ens_dim, cur_time_dim),
#'                                  missval = ifelse(vars$prec[i] == 'float', fillvalue_float, fillvalue_int),
#'                                  longname = vars$longname[i],
#'                                  prec = vars$prec[i])
#'     }else{
#'       def_list[[i]] <- ncvar_def(name =  vars$param[i],
#'                                  units = vars$units[i],
#'                                  dim = list(cur_loc_dim, ens_dim),
#'                                  missval = ifelse(vars$prec[i] == 'float', fillvalue_float, fillvalue_int),
#'                                  longname = vars$longname[i],
#'                                  prec = vars$prec[i])
#'     }
#'
#'   }
#'
#'   nc_name_out = scipiper::as_data_file(nc_name_out_ind)
#'
#'   if(file.exists(nc_name_out)){
#'     if(overwrite){
#'       file.remove(nc_name_out)
#'       ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
#'     }else{stop('cannot overwrite nc output file')}
#'   }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}
#'
#'   #Global file metadata
#'   ncatt_put(nc = ncout,
#'             varid = 0,
#'             attname = "project_id",
#'             attval = as.character(project_id),
#'             prec =  "text")
#'
#'   nc_close(ncout)
#'   gd_put(remote_ind = nc_name_out_ind, local_source = nc_name_out, config_file = gd_config)
#' }
#'
#'
#'
#' #' insert parameters by drawing from distribution
#' #'
#' nc_params_put = function(vars,
#'                          n_en,
#'                          nc_file_ind){
#'
#'   ncout <- nc_open(sc_retrieve(nc_file_ind, remake_file = 'getters.yml'), write = T)
#'
#'   param_names = vars$param
#'
#'   for(i in seq_along(param_names)){
#'
#'     cur_var = ncout$var[[param_names[i]]]
#'     varsize = cur_var$varsize
#'     # dims [loc_dim, ens_dim, time_dim]; position of dimensions following:
#'     loc_pos = 1
#'     ens_pos = 2
#'     time_pos = 3
#'
#'     n_dims = cur_var$ndims
#'
#'     cur_mean = vars$mean[i]
#'     cur_sd = vars$sd[i]
#'     cur_rparam = rnorm(n = n_en, mean = cur_mean, sd = cur_sd) # draw params from distribution; setting all segments to these param draws
#'     # check to make sure params are within min-max range
#'     cur_rparam = ifelse(cur_rparam < vars$min[i], vars$min[i], cur_rparam)
#'     cur_rparam = ifelse(cur_rparam > vars$max[i], vars$max[i], cur_rparam)
#'     cur_rparam = array(rep(cur_rparam, each =varsize[1]), dim = varsize)
#'
#'     start = rep(1, n_dims)
#'     # start[time_pos] = # need to update if there are month based parameters
#'
#'     count = varsize
#'     # count[time_pos] = 1 # adding only one issue time step
#'
#'     ncvar_put(nc = ncout,
#'               varid = param_names[i],
#'               vals = cur_rparam,
#'               start = start,
#'               count = count)
#'
#'   }
#'   nc_close(ncout)
#' }
#'
#' #' function for returning parameters; returns list
#' #'
#' nc_params_get = function(nc_file_ind,
#'                          param_names = NULL,
#'                          model_idxs = NULL,
#'                          ens = NULL,
#'                          months = NULL){
#'
#'   nc = nc_open(sc_retrieve(nc_file_ind, remake_file = 'getters.yml'), write = T)
#'
#'   if(!is.null(param_names)){
#'     param_names = param_names
#'   }else{
#'     param_names = names(nc$var)
#'   }
#'
#'   # dims [loc_dim, ens_dim, time_dim]; position of dimensions following:
#'   loc_pos = 1
#'   ens_pos = 2
#'   time_pos = 3
#'
#'   out_list = vector(mode = 'list', length = length(param_names))
#'   for(i in seq_along(param_names)){
#'     cur_var = nc$var[[param_names[i]]]
#'     varsize = cur_var$varsize
#'     n_dims = cur_var$ndims
#'
#'     all_model_idxs = as.character(cur_var$dim[[loc_pos]]$vals)
#'     all_ens = as.integer(cur_var$dim[[ens_pos]]$vals)
#'     if(n_dims > 2){
#'       all_months = cur_var$dim[[time_pos]]$vals
#'       # return all values, and then filter
#'       all_out = ncvar_get(nc = nc, varid = param_names[i]) %>% array(dim = varsize) %>%
#'         reshape2::melt(varnames = c('model_idx', 'ensemble', 'month')) %>%
#'         mutate(model_idx = all_model_idxs[model_idx],
#'                ensemble = all_ens[ensemble],
#'                month = all_months[month]) %>%
#'         rename(!!param_names[i] := value) %>%
#'         as_tibble()
#'     }else{
#'       # return all values, and then filter
#'       all_out = ncvar_get(nc = nc, varid = param_names[i]) %>% array(dim = varsize) %>%
#'         reshape2::melt(varnames = c('model_idx', 'ensemble')) %>%
#'         mutate(model_idx = all_model_idxs[model_idx],
#'                ensemble = all_ens[ensemble]) %>%
#'         rename(!!param_names[i] := value) %>%
#'         as_tibble()
#'     }
#'
#'     if(!is.null(model_idxs)){
#'       cur_model_idxs = as.character(model_idxs)
#'     }else{cur_model_idxs = as.character(all_model_idxs)}
#'     if(!is.null(ens)){
#'       cur_ens = as.integer(ens)
#'     }else{cur_ens = as.integer(all_ens)}
#'     if(n_dims > 2){
#'       if(!is.null(months)){
#'         cur_months = as.integer(months)
#'       }else{cur_months = as.integer(all_months)}
#'     }
#'
#'     if(n_dims > 2){
#'       out = dplyr::filter(all_out,
#'                           model_idx %in% cur_model_idxs,
#'                           ensemble %in% cur_ens,
#'                           month %in% cur_months)
#'     }else{
#'       out = dplyr::filter(all_out,
#'                           model_idx %in% cur_model_idxs,
#'                           ensemble %in% cur_ens)
#'     }
#'
#'     out_list[[i]] = out[,ncol(out)] %>% pull()
#'     names(out_list)[i] = param_names[i]
#'   }
#'
#'   nc_close(nc)
#'
#'   return(out_list)
#' }
#'
#'
#' # example function call:
#' # nc_create_drivers(hru_model_idxs = seq(1,765),
#' #                   forecast_horizon = 8,
#' #                   n_en = 20,
#' #                   issue_dates = dates,
#' #                   forecast_project_id = forecast_project_id,
#' #                   vars = drivers,
#' #                   nc_name_out = '2_2_model_drivers/out/forecasted_drivers.nc',
#' #                   overwrite = T)
#'
#'
#' nc_create_drivers = function(hru_model_idxs,
#'                              forecast_horizon,
#'                              n_en,
#'                              issue_dates,
#'                              forecast_project_id,
#'                              vars,
#'                              nc_name_out,
#'                              overwrite = T){
#'
#'   #Set dimensions
#'   ens <- as.integer(seq(1, n_en, 1))
#'   model_locations <- as.integer(hru_model_idxs)
#'   n_issue_date <- length(issue_dates)
#'   timestep <- as.integer(seq(0, n_issue_date - 1, 1)) # days since issue date #1
#'   forecast_days <- as.integer(seq(0, forecast_horizon - 1, 1))
#'
#'   ens_dim <- ncdim_def("ens",
#'                        units = "",
#'                        vals = ens,
#'                        longname = 'ensemble member')
#'   loc_dim <- ncdim_def("loc",
#'                        units = "",
#'                        vals = model_locations,
#'                        longname = 'hydrologic reach unit model index')
#'   time_dim <- ncdim_def("timestep",
#'                         units = '1 day',
#'                         longname = sprintf('Days since %s', issue_dates[1]),
#'                         vals = timestep)
#'   fdays_dim <- ncdim_def('forecast_days',
#'                          units = '1 day',
#'                          longname = 'Valid dates from issue time',
#'                          vals = forecast_days)
#'
#'   dim_nchar <- ncdim_def("nchar",
#'                          units = "",
#'                          vals = 1:nchar(as.character(issue_dates[1])),
#'                          create_dimvar = FALSE)
#'   ## quick check that units are valid
#'   udunits2::ud.is.parseable(ens_dim$units)
#'   udunits2::ud.is.parseable(loc_dim$units)
#'   udunits2::ud.is.parseable(time_dim$units)
#'   udunits2::ud.is.parseable(fdays_dim$units)
#'   udunits2::ud.is.parseable(dim_nchar$units)
#'
#'   #Define variables
#'   fillvalue <- 1e32
#'
#'   # use same dimensions as NOAA forecasts [lon, lat, forecast hours, ensemble, issue date]
#'   def_list <- list()
#'   # loop through variables that we're forecasting
#'   n_vars = length(vars$driver)
#'   for(i in 1:n_vars){
#'     def_list[[i]] <- ncvar_def(name =  vars$driver[i],
#'                                units = vars$units[i],
#'                                dim = list(loc_dim, fdays_dim, ens_dim, time_dim),
#'                                missval = fillvalue,
#'                                longname = vars$longname[i],
#'                                prec = vars$prec[i])
#'   }
#'
#'   def_list[[n_vars + 1]] <- ncvar_def(name = 'issue_time',
#'                                       units = 'datetime',
#'                                       dim = list(dim_nchar, time_dim),
#'                                       longname = 'Forecast issue time',
#'                                       prec = 'char')
#'
#'   if(file.exists(nc_name_out)){
#'     if(overwrite){
#'       file.remove(nc_name_out)
#'       ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
#'     }else{stop('cannot overwrite nc output file')}
#'   }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}
#'
#'   ncvar_put(nc = ncout,
#'             varid = def_list[[n_vars + 1]],
#'             vals = issue_dates)
#'
#'   #Global file metadata
#'   ncatt_put(nc = ncout,
#'             varid = 0,
#'             attname = "forecast_project_id",
#'             attval = as.character(forecast_project_id),
#'             prec =  "text")
#'
#'   nc_close(ncout)
#' }
#'
#'
#' nc_drivers_put = function(var_df,
#'                           var_name,
#'                           en,
#'                           issue_date,
#'                           nc_name_out){
#'
#'   ncout <- nc_open(nc_name_out, write = T)
#'
#'   cur_var = ncout$var[[var_name]]
#'   varsize = cur_var$varsize
#'   issue_dates = ncvar_get(ncout, varid = 'issue_time')
#'   # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
#'   loc_pos = 1
#'   fdays_pos = 2
#'   ens_pos = 3
#'   time_pos = 4
#'
#'   n_dims = cur_var$ndims
#'
#'   cur_issue_time = which(issue_date == issue_dates)
#'   cur_en = en
#'
#'   n_fdays = varsize[fdays_pos]
#'   n_en = varsize[ens_pos]
#'
#'   start = rep(1, n_dims)
#'   start[time_pos] = cur_issue_time
#'   start[ens_pos] = cur_en
#'
#'   count = varsize
#'   count[time_pos] = 1 # adding only one issue time step
#'   count[ens_pos] = 1 # adding output from only one ensemble
#'
#'   ncvar_put(nc = ncout,
#'             varid = var_name,
#'             vals = pull(var_df, var_name),
#'             start = start,
#'             count = count)
#'
#'   nc_close(ncout)
#' }
#'
#'
#' #' function for returning forecasted driver variables; returns tibble
#' #'
#' nc_drivers_get = function(nc_file,
#'                           var_name = c('tmin','tmax','prcp'), # default return all drivers
#'                           issue_dates = NULL,
#'                           model_idxs = NULL, # hru model idx
#'                           ens = NULL,
#'                           fdays = NULL){
#'
#'   nc = nc_open(nc_file)
#'
#'   # temp output dims [loc_dim, fdays_dim, ens_dim, time_dim]; position of dimensions following:
#'   loc_pos = 1
#'   fdays_pos = 2
#'   ens_pos = 3
#'   time_pos = 4
#'
#'   cur_var = nc$var[[1]]
#'
#'   all_issue_dates = as.Date(ncvar_get(nc, varid = 'issue_time')) # all possible issue dates
#'   all_model_idxs = as.character(cur_var$dim[[loc_pos]]$vals)
#'   all_ens = as.integer(cur_var$dim[[ens_pos]]$vals)
#'   all_valid_times = cur_var$dim[[fdays_pos]]$vals
#'
#'   nc_close(nc)
#'
#'   if(!is.null(issue_dates)){
#'     cur_issue_dates = which(all_issue_dates %in% as.Date(issue_dates)) - 1 # indexed starting with 0
#'   }else{
#'     cur_issue_dates = seq(0, length(all_issue_dates), 1)
#'   }
#'   if(!is.null(model_idxs)){
#'     cur_model_idxs = which(all_model_idxs %in% as.character(model_idxs))
#'   }else{
#'     cur_model_idxs = seq_along(all_model_idxs)
#'   }
#'   if(!is.null(ens)){
#'     cur_ens = which(all_ens %in% as.integer(ens))
#'   }else{
#'     cur_ens = seq_along(all_ens)
#'   }
#'   if(!is.null(fdays)){
#'     cur_fdays = which(all_valid_times %in% as.integer(fdays)) - 1 # indexed starting with 0
#'   }else{
#'     cur_fdays = seq(0, length(all_valid_times), 1)
#'   }
#'
#'   # return all values, and then filter
#'   out = tidync::tidync(x = nc_file) %>%
#'     tidync::activate(var_name) %>%
#'     tidync::hyper_filter(loc = loc %in% cur_model_idxs,
#'                  forecast_days = forecast_days %in% cur_fdays,
#'                  ens = ens %in% cur_ens,
#'                  timestep = timestep %in% cur_issue_dates) %>%
#'     tidync::hyper_tibble() %>%
#'     mutate(issue_time = all_issue_dates[timestep + 1],
#'            model_idx = all_model_idxs[loc],
#'            valid_time = issue_time + as.difftime(all_valid_times[forecast_days + 1], units = 'days'),
#'            ensemble = all_ens[ens]) %>%
#'     dplyr::select(model_idx, valid_time, ensemble, issue_time, var_name)
#'
#'   return(out)
#' }
#'

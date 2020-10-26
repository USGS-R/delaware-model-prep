
get_sntemp_discharge = function(model_output_file, model_fabric_file){

  model_output = read.csv(model_output_file, header = T, stringsAsFactors = F)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key = 'model_idx', value = 'discharge', starts_with('X')) %>%
    mutate(model_idx = gsub('X', '', model_idx)) %>%
    rename(date = Date) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, discharge) %>%
    arrange(as.numeric(model_idx))

  return(model_output)
}

get_sntemp_temperature = function(model_output_file, model_fabric_file){

  model_output = read.csv(model_output_file, header = T, stringsAsFactors = F)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key = 'model_idx', value = 'water_temp', starts_with('X')) %>%
    mutate(model_idx = gsub('X', '', model_idx)) %>%
    rename(date = Date) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, water_temp) %>%
    arrange(as.numeric(model_idx))

  return(model_output)
}

get_sntemp_intermediates = function(model_output_file,
                                    model_fabric_file,
                                    sntemp_vars){

  ### This was given error with updated model output and reading in the data
  # error was:
  #  Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  :
  #       line 1489316 did not have 20 elements
  # even though all rows had 20 elements from manual inspection of R-indicated problem rows
  # model_output = read.table(model_output_file, header = T, stringsAsFactors = F) %>%
  #    dplyr::slice(-1) # first row indicates column type

  # using fread to solve error documented above ^
  to_skip = length(sntemp_vars) + 6 # how many lines to skip when reading in (based on how many vars are output)
  model_output = data.table::fread(file = model_output_file,
                                   skip = to_skip, header = F)
  # head(model_output)

  cols = readr::read_delim(file = model_output_file, delim = '\t', n_max = 2, skip = to_skip)

  colnames(model_output) = colnames(cols)
  rm(cols)
  # model_otuput = read.csv(model_output_file,sep = ' ', skip = 20, header = T, stringsAsFactors = F)
  #
  # fc = file(file.path(model_output_file))
  # ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  # close(fc)

  model_fabric = sf::read_sf(model_fabric_file)

  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat),
                   model_idx = as.character(model_fabric$model_idx))

  model_output = model_output %>% as_tibble() %>%
    mutate(timestamp = as.Date(timestamp),
           nsegment = as.character(nsegment)) %>%
    rename(date = timestamp,
           model_idx = nsegment) %>%
    gather(key = 'parameter', value = 'parameter_value', starts_with('seg')) %>%
    mutate(parameter_value = as.numeric(parameter_value)) %>%
    left_join(seg_ids, by = 'model_idx') %>%
    select(seg_id_nat, model_idx, date, parameter, parameter_value) %>%
    arrange(as.numeric(model_idx))

  return(model_output)
}


get_sntemp_initial_states = function(state_names,
                                     by_seg = T,
                                     seg_model_idxs = NULL, # IC of segments to return
                                     model_fabric_file = '20191002_Delaware_streamtemp/GIS/Segments_subset.shp',
                                     state_order_file = '4_model/cfg/state_order.rds',
                                     model_run_loc = '4_model/tmp',
                                     ic_file = 'prms_ic.txt'){
  # order of the states in the ic file - PRMS-SNTemp ic file isn't documented so these are the order of the states AS LONG AS
  #  we use the same modules every time
  state_order = readRDS(state_order_file)

  # open the ic file
  fc = file(file.path(model_run_loc, ic_file))
  ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  close(fc)

  if(by_seg){ #states are per segment
    model_fabric = sf::read_sf(model_fabric_file)

    seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
      arrange(as.numeric(model_idx))

    out = seg_ids

    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      cur_state_vals = na.omit(as.numeric(ic[[cur_state_row]]))

      out = out %>%
        mutate(temp_name = cur_state_vals) %>%
        rename(!!noquote(cur_state) := temp_name)
    }
    if(!is.null(seg_model_idxs)){
      out = dplyr::filter(out, model_idx %in% seg_model_idxs)
    }
  }else{
    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      cur_state_vals = na.omit(as.numeric(ic[[cur_state_row]]))

      out = tibble(!!noquote(cur_state) := cur_state_vals) # temporary fix for sesnativity testing

      # out = out %>%
      #   mutate(temp_name = cur_state_vals) %>%
      #   rename(!!noquote(cur_state) := temp_name)
    }
  }

  return(out)
}


# function for retrieving parameters from param file
get_sntemp_params = function(param_names,
                             model_run_loc,
                             model_fabric_file = 'GIS/Segments_subset.shp',
                             param_file = 'input/myparam.param',
                             param_default_file = 'control/delaware.control.par_name',
                             n_segments = 456){

  params = readLines(file.path(model_run_loc, param_file))

  model_fabric = sf::read_sf(file.path(model_run_loc, model_fabric_file))

  # order by model_idx
  seg_ids = tibble(seg_id_nat = as.character(model_fabric$seg_id_nat), model_idx = as.character(model_fabric$model_idx)) %>%
    arrange(as.numeric(model_idx))

  out = vector(mode = 'list', length = length(param_names))

  if(length(param_names) == 0){
    out = out
  }else{
    for(i in seq_along(param_names)){

      defaults = get_default_param_vals(param_name = param_names[i],
                                        model_run_loc = model_run_loc,
                                        param_default_file = param_default_file)

      param_loc_start = which(params == param_names[i]) + 4 + as.numeric(defaults$ndim)
      param_loc_end = param_loc_start + as.numeric(defaults$size) - 1

      cur_param_vals = params[param_loc_start:param_loc_end]

      out[[i]] = cur_param_vals
      names(out)[i] = param_names[i]
    }
  }

  return(out)
}


#pulling out parameters based on segment id's (based on model index);
get_params_by_segment = function(param_names,
                                 model_run_loc,
                                 seg_model_idxs, # segment model idxs to calibrate
                                 model_fabric_file = 'GIS/Segments_subset.shp',
                                 param_file = 'input/myparam.param',
                                 param_default_file = 'control/delaware.control.par_name',
                                 n_segments = 456,
                                 n_hrus = 765,
                                 n_gwr = 765,
                                 n_ssr = 765){

  params = get_sntemp_params(param_names = param_names,
                             model_run_loc = model_run_loc,
                             model_fabric_file = model_fabric_file,
                             param_file = param_file,
                             param_default_file = param_default_file)

  seg_model_idxs = sort(as.numeric(seg_model_idxs))
  hru_model_idxs = get_segment_hrus(seg_model_idxs = as.character(seg_model_idxs),
                                    model_run_loc = model_run_loc)  # getting hru_model_idxs based on seg_id

  for(i in seq_along(param_names)){
    cur_defaults = get_default_param_vals(param_name = param_names[i],
                                          model_run_loc = model_run_loc,
                                          param_default_file = param_default_file)
    # figure out dimensions, and pull out parameters associated with segments
    if(cur_defaults$dim == 'one'){
      params[[param_names[i]]] = tibble(dim = 'one' , vals = params[[param_names[i]]])
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nsegment'){ # a stream segment-based parameter
      # it will be ordered by segment model_idx (e.g. 1, 2, ..., nsegments)
      params[[param_names[i]]] = tibble(seg_model_idx = as.character(seg_model_idxs),
                                        vals = params[[param_names[i]]][seg_model_idxs])
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nhru'){
      # it will be ordered by hru model_idx (e.g. 1, 2, ..., nhru)
      params[[param_names[i]]] = tibble(hru_model_idx = hru_model_idxs$hru_model_idxs,
                                        seg_model_idx = hru_model_idxs$seg_model_idxs,
                                        vals = params[[param_names[i]]][as.numeric(hru_model_idxs$hru_model_idxs)])
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'ngw'){
      # it will be ordered by hru model_idx (e.g. 1, 2, ..., nhru) which corresponds to gw reservoir
      params[[param_names[i]]] = tibble(hru_model_idx = hru_model_idxs$hru_model_idxs,
                                        seg_model_idx = hru_model_idxs$seg_model_idxs,
                                        vals = params[[param_names[i]]][as.numeric(hru_model_idxs$hru_model_idxs)])
    }else if(cur_defaults$ndim == '1' & cur_defaults$dim == 'nssr'){
      # it will be ordered by hru model_idx (e.g. 1, 2, ..., nhru) which corresponds to gw reservoir
      params[[param_names[i]]] = tibble(hru_model_idx = hru_model_idxs$hru_model_idxs,
                                        seg_model_idx = hru_model_idxs$seg_model_idxs,
                                        vals = params[[param_names[i]]][as.numeric(hru_model_idxs$hru_model_idxs)])
    }else if(cur_defaults$dim == 'nmonths'){
      params[[param_names[i]]] = tibble(dim = rep('nmonths', as.numeric(cur_defaults$size)),
                                                  vals = params[[param_names[i]]])
    }else if(cur_defaults$ndim == '2'){
      if(grepl('nsegment', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
        # per segment x month basis is organized in order of segment model_idx and then month
        #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 455_Dec, 456_Dec
        idxs = rep(seq(1,n_segments), 12)
        params[[param_names[i]]] = tibble(seg_model_idx = rep(as.character(seg_model_idxs), 12),
                                          month = rep(as.character(seq(1,12)), each = length(seg_model_idxs)),
                                          vals = params[[param_names[i]]][which(as.character(idxs) %in% as.character(seg_model_idxs))])
      }else if(grepl('nhru', cur_defaults$dim) & grepl('nmonths', cur_defaults$dim)){
        # per hru x month basis is organized in order of hru model_idx and then month
        #   - e.g. 1_Jan, 2_Jan, ...., 1_Feb, 2_Feb, .... 755_Dec, 756_Dec
        idxs = rep(seq(1,n_hrus), 12)
        params[[param_names[i]]] = tibble(hru_model_idx = rep(hru_model_idxs$hru_model_idxs, 12),
                                          seg_model_idx = rep(hru_model_idxs$seg_model_idxs, 12),
                                          month = rep(as.character(seq(1,12)), each = length(hru_model_idxs$hru_model_idxs)),
                                          vals = params[[param_names[i]]][which(as.character(idxs) %in% as.character(hru_model_idxs$hru_model_idxs))])
      }
    }
  }

  return(params)
}


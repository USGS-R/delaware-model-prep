
#' update parameters
#'
update_sntemp_params = function(param_names,
                                updated_params, # list of parameters
                                model_run_loc = '4_model/tmp',
                                param_file = 'input/myparam.param',
                                param_default_file = 'control/delaware.control.par_name',
                                n_segments = 456){

  params = readLines(file.path(model_run_loc, param_file))

  for(i in seq_along(param_names)){
    # add in default param in not in param file
    if(length(which(params == param_names[i])) == 0){
      add_default_sntemp_params(param_names = param_names[i],
                                model_run_loc = model_run_loc,
                                param_file = param_file,
                                param_default_file = param_default_file)
      params = readLines(file.path(model_run_loc, param_file))
    }

    defaults = get_default_param_vals(param_name = param_names[i],
                                      model_run_loc = model_run_loc,
                                      param_default_file = param_default_file)


    param_loc_start = which(params == param_names[i]) + 4 + as.numeric(defaults$ndim)
    param_loc_end = param_loc_start + as.numeric(defaults$size) - 1

    cur_vals = updated_params[[param_names[i]]]

    if(defaults$type == '1'){ # long
      params[param_loc_start:param_loc_end] = as.character(round(as.numeric(cur_vals), digits = 0))
    }else if(defaults$type == '2'){ # float
      params[param_loc_start:param_loc_end] = as.character(round(as.numeric(cur_vals), digits = 6))
    }
  }

  writeLines(params, file.path(model_run_loc, param_file))
}

#' update jh_coef
#'
update_jh_coef = function(updated_params,
                          model_run_loc = '4_model/tmp',
                          param_file = 'input/myparam.param',
                          n_hrus = 765,
                          n_months = 12){

  params = readLines(file.path(model_run_loc, param_file))

  param_loc_start = which(params == 'jh_coef') + 6
  param_loc_end = param_loc_start + n_hrus * n_months - 1

  params[param_loc_start:param_loc_end] = as.character(round(as.numeric(updated_params), digits = 6))

  writeLines(params, file.path(model_run_loc, param_file))
}

#' update lat_temp_adj
#'
update_lat_temp_adj = function(updated_params,
                               model_run_loc = '4_model/tmp',
                               param_file = 'input/myparam.param',
                               n_segments = 456,
                               n_months = 12){

  params = readLines(file.path(model_run_loc, param_file))

  param_loc_start = which(params == 'lat_temp_adj') + 6
  param_loc_end = param_loc_start + n_segments * n_months - 1

  params[param_loc_start:param_loc_end] = as.character(round(as.numeric(updated_params), digits = 3))

  writeLines(params, file.path(model_run_loc, param_file))
}

#' update states
#'
update_sntemp_states = function(state_names,
                                by_seg = T,
                                seg_model_idxs = NULL, # segments to update
                                updated_states,
                                state_order_file = '4_model/cfg/state_order.rds',
                                model_run_loc = '4_model/tmp',
                                ic_file_in = 'prms_ic.txt',
                                ic_file_out = 'prms_ic.txt',
                                n_segments = 456){
  # order of the states in the ic file - PRMS-SNTemp ic file isn't documented so these are the order of the states AS LONG AS
  #  we use the same modules every time
  state_order = readRDS(state_order_file)

  if(!is.null(seg_model_idxs)){
    n_segments = length(seg_model_idxs)
  }

  # open the ic file
  fc = file(file.path(model_run_loc, ic_file_in))
  ic = strsplit(readLines(fc, skipNul = T), ' +') # reading in text with irregular white space seperators
  close(fc)

  # update the states
  if(by_seg){
    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      cur_updated_states = tibble(vals = as.character(updated_states[((i-1)*n_segments+1):(i*n_segments)]), model_idx = seg_model_idxs)

      prior_states = stringi::stri_remove_empty(ic[[cur_state_row]]) # removes blank string

      cur_updated_states_vec = prior_states

      cur_updated_states_vec[as.numeric(cur_updated_states$model_idx)] = cur_updated_states$vals

      ic[[cur_state_row]] = cur_updated_states_vec
    }
  }else{
    for(i in 1:length(state_names)){

      cur_state = state_names[i]
      cur_state_row = state_order$row_idx[state_order$state_name == cur_state]

      ic[[cur_state_row]] = as.character(updated_states) # quick fix for sensativity testing
    }
  }

  out = lapply(X = 1:length(ic), FUN = function(x){paste(ic[[x]], collapse = ' ')})

  # have to delete old ic file before writing new one because of appending thing that I do below
  file.remove(file.path(model_run_loc, ic_file_out))

  # write out new ic file
  lapply(out, write, file.path(model_run_loc, ic_file_out), append = T)
}


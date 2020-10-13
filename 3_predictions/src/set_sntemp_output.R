

set_sntemp_output = function(output_names,
                             model_run_loc,
                             control_file = 'delaware.control'){

  ctrl = readLines(file.path(model_run_loc, 'control', control_file))

  # location of previous output variables
  output_loc_start = grep('aniOutVar_names', ctrl) + 3
  output_loc_end = grep('ani_output_file', ctrl) - 2

  if(ctrl[output_loc_start] == '####'){ # if no output variables previously
    new_ctrl = ctrl

    new_output_loc_start = grep('aniOutVar_names', new_ctrl) + 3

    new_ctrl[grep('aniOutVar_names', new_ctrl) + 1] = as.character(length(output_names))

    new_ctrl[grep('naniOutVars', new_ctrl) + 3] = as.character(length(output_names))

    new_ctrl = c(new_ctrl[1:(new_output_loc_start-1)], output_names, new_ctrl[new_output_loc_start:length(new_ctrl)])
  }else{
    new_ctrl = ctrl[-c(output_loc_start:output_loc_end)] # get rid of old output variables

    new_output_loc_start = grep('aniOutVar_names', new_ctrl) + 3

    new_ctrl[grep('aniOutVar_names', new_ctrl) + 1] = as.character(length(output_names))

    new_ctrl[grep('naniOutVars', new_ctrl) + 3] = as.character(length(output_names))

    new_ctrl = c(new_ctrl[1:(new_output_loc_start-1)], output_names, new_ctrl[new_output_loc_start:length(new_ctrl)])
  }

  writeLines(new_ctrl, file.path(model_run_loc, 'control', control_file))
  # gd_put(remote_ind = out_ind,
  #        local_source = file.path(model_run_loc, 'control', control_file),
  #        dry_put = T)
}



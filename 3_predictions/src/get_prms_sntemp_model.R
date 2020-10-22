
#' @param gd_zip_ind_file ind file
#' @param unzip_loc location of where model with be unzipped
#' @param gd_config google drive configuration
get_prms_sntemp_model = function(out_ind,
                                 gd_zip_ind_file,
                                 unzip_loc,
                                 orig_model_loc,
                                 model_run_loc,
                                 gd_config = 'lib/cfg/gd_config.yml'){

  sc_retrieve(gd_zip_ind_file, 'getters.yml')

  unzip(zipfile = scipiper::as_data_file(gd_zip_ind_file),
        overwrite = T,
        exdir = unzip_loc)

  copy_model_to_run_dir(model_run_loc = model_run_loc,
                        orig_model_loc = orig_model_loc)

  sc_indicate(ind_file = out_ind)
}

#' @param model_run_loc directory location where the model will be run
#' @param orig_model_loc original model location; useful to have this as an unaltered model location and model_run_loc as a place where you can change parameters / states / inputs
copy_model_to_run_dir = function(model_run_loc,
                                 orig_model_loc){
  # copy over original run files to temporary file location
  dir.create(model_run_loc, showWarnings = F)
  print('Copying original model files to model working directory...')
  files_to_transfer = list.files(orig_model_loc)
  file.copy(from = file.path(orig_model_loc, files_to_transfer),
            to = model_run_loc,
            overwrite = T,
            recursive = T)

}


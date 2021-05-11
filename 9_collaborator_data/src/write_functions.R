write_to_csv <- function(dat_ind, out_ind) {
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  readr::write_csv(dat, as_data_file(out_ind))
  gd_put(out_ind)
}

filter_subset <- function(dat_ind, subnet_ind, out_ind) {
  dat <- readRDS(sc_retrieve(dat_ind, 'getters.yml'))
  subnet <- readRDS(sc_retrieve(subnet_ind, 'getters.yml'))

  subnet_dat <- filter(dat, seg_id_nat %in% unique(subnet$edges$seg_id_nat))

  readr::write_csv(subnet_dat, as_data_file(out_ind))
  gd_put(out_ind)
}

get_res_features <- function(dat_ind, out_ind) {
  res <- readRDS(sc_retrieve(dat_ind, 'getters.yml')) %>%
    select(GRAND_ID, AREA_SKM, RES_NAME, DAM_NAME, ALT_NAME, RIVER, ALT_RIVER, YEAR, DAM_HGT_M, DAM_LEN_M, DEPTH_M, ELEV_MASL, CATCH_SKM) %>%
    distinct()

  readr::write_csv(res, as_data_file(out_ind))
  gd_put(out_ind)
}

# save distance matrices in numpy format
save_dist_matrices <- function(dist_mat_ind, out_ind) {
  np <- reticulate::import('numpy')

  dist_mat_list <- readRDS(sc_retrieve(dist_mat_ind, 'getters.yml'))

  out_file <- as_data_file(out_ind)
  np$savez_compressed(
    file=out_file,
    downstream=dist_mat_list$downstream,
    upstream=dist_mat_list$upstream,
    complete=dist_mat_list$complete,
    updown=dist_mat_list$updown,
    rowcolnames=rownames(dist_mat_list$downstream))
  # R access examples:
  # loaded <- np$load('out/dists.npz')
  # loaded$files
  # loaded$f[['updown']]

  gd_put(out_ind)


}

write_zarr_tarfile <- function(dat_ind, out_ind){
  out_file <- as_data_file(out_ind)
  write_py <- reticulate::import_from_path('write_functions_py', path='9_collaborator_data/src/')
  data_file <- sc_retrieve(dat_ind, 'getters.yml')
  write_py$write_zarr_tar(
    data_file,
    as_data_file(out_ind)
  )
  gd_put(out_ind)
}

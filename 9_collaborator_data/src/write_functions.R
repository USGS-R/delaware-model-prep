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
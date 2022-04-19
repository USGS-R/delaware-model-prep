get_sb_item <- function(sb_id, sb_names, out_ind) {
  sbtools::authenticate_sb()
  out_file <- scipiper::as_data_file(out_ind)
  sbtools::item_file_download(sb_id = sb_id, names = sb_names, destinations = out_file, overwrite_file = TRUE)
  gd_put(out_ind)

}

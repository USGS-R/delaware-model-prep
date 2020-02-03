# get national geofabric
# placeholder, need to write

get_national_gf <- function(sb_id, sb_name, out_file) {
  temp_loc <- tempdir()
  sbtools::item_file_download(sb_id = sb_id, names = sb_name, dest_dir = temp_loc)
  unzip()
}

# get national geofabric
# placeholder, need to write

get_national_gf <- function(sb_id, sb_name, out_ind) {

  # check against the local hash, if already present, and don't download if not needed
  out_dir <- scipiper::as_data_file(out_ind)
  if(dir.exists(out_dir) && file.exists(out_ind)) {
    current_files <- tools::md5sum(dir(out_dir, full.names=TRUE))
    expected_files <- unlist(yaml::yaml.load_file(out_ind))
    if(all.equal(current_files, expected_files)) {
      message('GF is already downloaded and matches the .ind file; doing nothing')
      return()
    } else {
      stop("GF is downloaded but doesn't match the .ind file. Stopping to let you manually update/delete one or both to save time (it's a big download!)")
    }
  }

  # if the data don't yet exist, download and unzip. if they do already exist,
  # then either (1) the ind file also exists and we will have returned or
  # stopped above, or (2) the ind file doesn't exist and will get created below.
  # we'll post a message if it's case 2.
  if(!dir.exists(out_dir)) {
    temp_loc <- tempfile()
    sbtools::item_file_download(sb_id = sb_id, names = sb_name, destinations = temp_loc)
    if(dir.exists(out_dir)) unlink(out_dir, recursive=TRUE)
    unzip(temp_loc, exdir=dirname(out_dir))
  } else {
    message('GF is already downloaded; creating an .ind file to match')
  }

  # write an ind file with hash for each file in the directory
  sc_indicate(out_ind, data_file=dir(out_dir, full.names=TRUE))
}

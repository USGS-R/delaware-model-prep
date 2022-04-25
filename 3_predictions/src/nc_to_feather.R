extract_sntemp_nc <- function(in_ind, out_ind, firstdate) {

  first_date = as.Date(firstdate) # first model date, used to calculate valid dates below

  out = tidync::tidync(sc_retrieve(in_ind)) %>%
    tidync::hyper_tibble() %>%
    mutate(time = time + first_date,
           seg_id_nat = as.character(seg_id_nat))

  arrow::write_feather(out, as_data_file(out_ind))
  gd_put(out_ind)
}

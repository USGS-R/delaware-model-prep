# drb_filtered_sites is an existing target in the pipeline that contains the
# temperature sites that have been filtered to bird dist and fish dist
# (2_observations/out/drb_filtered_sites.rds.ind)

# Read in NHDPlusV2 - NHM crosswalk table from drb-network-prep
fetch_xwalk <- function(out_ind, url) {
  GFv1_NHDv2_xwalk_url <- url
  crosswalk <- readr::read_csv(GFv1_NHDv2_xwalk_url, col_types = 'cccc')
  readr::write_csv(crosswalk, as_data_file(out_ind))
  gd_put(out_ind)
}

# Download ref-gages database from https://github.com/internetofwater/ref_gages/releases/
fetch_refgages <- function(out_ind, url) {
  download.file(url,
                destfile = as_data_file(out_ind))
  gd_put(out_ind)
}
site_to_reach_flags <- function(out_ind, sites_ind, cross_ind, refgages_ind) {

  # Add seg_match QC flags to drb_filtered_sites data frame
  drb_filtered_sites <- readRDS(sc_retrieve(sites_ind))
  crosswalk <- readr::read_csv(sc_retrieve(cross_ind))
  ref_gages_v0.5 <- sf::st_read(sc_retrieve(refgages_ind), quiet = TRUE)

  drb_filtered_sites_QC <- flag_seg_matches(drb_filtered_sites,
                                            bird_dist_cutoff_m = 250,
                                            crosswalk = crosswalk,
                                            ref_gages = ref_gages_v0.5)



  saveRDS(drb_filtered_sites_QC, file = as_data_file(out_ind))
  gd_put(out_ind)

}

check_new_flags <- function(out_ind, siteqa_ind, manualqa_ind) {

  # The file 2_observations/in/drb_filtered_sites_seg_match_QC.csv was manually created to
  # log site QC decisions based on visual inspection of the sites that were flagged according
  # to the criteria implemented in flag_seg_matches()
  # we want to throw a warning and write a site difference file that shows which sites
  # should be manually inspected

  # check flagged sites against manually checked sites from last run
  # if there are flagged sites that weren't manually checked, send a warning message
  manual_check <- readr::read_csv(sc_retrieve(manualqa_ind))
  # compare and write a file of sites that are not in old flag file
  # that Lauren manually checked. These sites should be checked to determine
  # whether to keep or throw out matches.
  new_qa <- readRDS(sc_retrieve(siteqa_ind))

  compare <- new_qa %>%
    filter(seg_match_flags_count > 0) %>%
    filter(!site_id %in% manual_check$site_id)

  # throw a message and write file
  if (nrow(compare) < 1) {
    message('ALL GOOD HERE: all flagged site-to-reach matches have been manually inspected.')
  } else {
    message(sprintf('WARNING: some flagged site-to-reach matches have not been manually inspected. See file %s for the list of sites to inspect.', as_data_file(manualqa_ind)))
  }
  saveRDS(compare, as_data_file(out_ind))
  gd_put(out_ind)
}

remove_bad_matches <- function(out_ind, manualqa_ind, sites_ind) {
  sites <- readRDS(sc_retrieve(sites_ind))

  # find flagged + manually inspected sites that Lauren recommends dropping
  drop_sites <- readr::read_csv(sc_retrieve(manualqa_ind)) %>%
    filter(seg_match_recommendation %in% 'drop')

  # filter out "drop" sites
  out_sites <- sites %>%
    filter(!site_id %in% drop_sites$site_id)

  saveRDS(out_sites, as_data_file(out_ind))
  gd_put(out_ind)
}




#' Function to append segment metadata from NHDPlus and NWIS to sites data frame
#'
#' @param sites_df data frame representing the dataset of site locations within the DRB,
#' already filtered using specified bird distance or fish distance cutoff values. sites_df
#' contains columns site_id, longitude, and latitude.
#' @bird_dist_cutoff_m integer indicating the bird distance cutoff used to filter the
#' site-to-reach matches.
#' @param crosswalk data frame containing columns PRMS_segid and comid, with one row
#' per comid.
#' @param ref_gages sf object representing the ref-gages dataset, downloaded from
#' https://github.com/internetofwater/ref_gages. ref_gages should contain the
#' columns provider_id, name, provider, nhdpv2_COMID.
#'
#' @return returns a data frame containing the original sites data frame, with additional
#' attributes gathered from NHDPlus, NWIS, and ref-gages that can be used to QA/QC
#' site-to-reach matches.
#'
append_seg_match_metadata <- function(sites_df, bird_dist_cutoff_m, crosswalk, ref_gages){

  # Fetch NHDV2 flowlines for given comids;
  # chunk the fetch step by PRMS_segid to prevent timeout issues.
  message("Fetching NHDv2 flowlines for comids in crosswalk...")
  start_time <- Sys.time()
  nhd_flines <- crosswalk %>%
    split(.,.$PRMS_segid) %>%
    purrr::map_dfr(.,.f = function(x){

      nhdplusTools::get_nhdplus(comid = x$comid,
                                realization = 'flowline') %>%
        select(comid, gnis_name, streamorde, streamcalc, totdasqkm, reachcode, tomeas, frommeas)

    })
  end_time <- Sys.time()
  elapsed_time <- difftime(end_time, start_time, units = "secs")
  message(sprintf("Fetched %s NHDv2 flowlines in %s sec",
                  length(nhd_flines$comid), round(elapsed_time, 1)))

  # Project the downloaded NHDPlusV2 flowlines
  nhd_flines_proj <- st_transform(nhd_flines, 5070)

  # Convert sites_df to an sf object and transform to match projection of NHD flowlines
  sites_sf <- sites_df %>%
    sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    st_transform(st_crs(nhd_flines_proj))

  # Match sites to NHDPlusV2 COMID (match up to 3 COMID's within search radius)
  pts_w_comid_mult_matches <- nhdplusTools::get_flowline_index(flines = nhd_flines_proj,
                                                               points = sites_sf,
                                                               # return up to 3 comid matches within radius
                                                               max_matches = 3,
                                                               # expand search radius beyond bird_dist_cutoff
                                                               search_radius = bird_dist_cutoff_m*4,
                                                               # return matched values using precision = 1 m
                                                               precision = 1)

  # Of the COMIDs returned from the nearest neighbor search, return the closest matched NHDPlusV2 COMID
  pts_w_comid <- pts_w_comid_mult_matches %>%
    group_by(id) %>%
    arrange(offset) %>%
    slice(1) %>%
    ungroup()

  # nhdplusTools returns an "id" column which is just an index from 1 to
  # the number of sites. To later join to the sites data, we need to add
  # a matching index column.
  sites_df_w_id <- rowid_to_column(sites_df, "id")

  # Append matched NHDPlusV2 comids to sites_df and format columns
  sites_w_comid <- sites_df_w_id %>%
    left_join(pts_w_comid, by = "id") %>%
    rename(comid_nearest_match = COMID, comid_match_dist_m = offset) %>%
    select(-c(id,REACHCODE, REACH_meas)) %>%
    # get upstream watershed area for comid_nearest_match from NHDPlusV2 VAA
    left_join(., nhd_flines %>%
                sf::st_drop_geometry() %>%
                select(comid,gnis_name, streamorde,streamcalc,totdasqkm),
              by = c("comid_nearest_match" = "comid")) %>%
    rename(totdasqkm_comid_match = totdasqkm,
           name_comid_match = gnis_name,
           streamorde_comid_match = streamorde,
           streamcalc_comid_match = streamcalc)

  # Subset USGS gage locations from sites_df and fetch site metadata from NWIS
  usgs_gages <- sites_df %>%
    # create a new column, gage_id, that contains only the gage number
    mutate(gage_id = str_replace(site_id, "USGS-","")) %>%
    # subset sites containing the prefix "USGS-"
    filter(substr(site_id, 1, 5) == "USGS-")

  usgs_site_info <- dataRetrieval::readNWISsite(c(usgs_gages$gage_id)) %>%
    select(site_no, drain_area_va) %>%
    mutate(site_id = paste0("USGS-",site_no)) %>%
    # convert reported drainage area from mi^2 to km^2
    mutate(totdasqkm_nwis = drain_area_va*2.58999) %>%
    select(-site_no)

  # Format attribute columns in ref-gages database
  ref_gages_info <- ref_gages %>%
    sf::st_drop_geometry() %>%
    select(provider_id, name, provider, nhdpv2_COMID) %>%
    rename(nhdpv2_COMID_refgages = nhdpv2_COMID,
           name_refgages = name,
           provider_refgages = provider)

  # Append NWIS metadata and ref-gages information to sites data frame containing comid's
  sites_out <- sites_w_comid %>%
    left_join(usgs_site_info, by = "site_id") %>%
    mutate(area_diff_km2 = abs(totdasqkm_comid_match - totdasqkm_nwis)) %>%
    left_join(ref_gages_info, by = c("site_id" = "provider_id"))

  return(sites_out)

}



#' Function to flag site-to-reach matches based on a set of criteria that uses information
#' garnered from additional datasets as well as site metadata.
#'
#' @details Criteria used to assign site-to-reach QC flags:
#' 1. For all sites, use the NHM-NHDPlusV2 crosswalk table to snap the site location to the
#' nearest NHDPlusV2 flowline reach. Flag sites where the matched NHDPlusV2 COMID is not
#' within the COMIDs that intersect the NHM as given in the crosswalk table (comid_seg).
#'
#' 2. For NWIS sites, compare the upstream area of the nearest NHDPlusV2 COMID with the
#' upstream area reported in NWIS. Flag sites with a discrepancy of >50 km^2 between the two areas.
#'
#' 3. For NWIS sites, check whether the site is included in the USGS ref-gages database and
#' flag sites where the associated NHDPlusV2 COMID is NA in ref-gages. nhdpv2_comid equal to
#' NA suggests that the gage site is not located on a known NHDPlusV2 flowline, and thus not
#' located on the NHM network.
#'
#' 4. For NWIS sites, check whether the site is included in the USGS ref-gages database and
#' flag sites where the associated NHDPlusV2 COMID does not intersect the NHM network.
#'
#' 5. For all sites, flag sites with the string "trib" in the site name, which may indicate
#' that the site is co-located with a nearby tributary string rather than the river segment
#' represnted in the NHM network.
#'
#' @param sites_df data frame representing the dataset of site locations within the DRB,
#' already filtered using specified bird distance or fish distance cutoff values. sites_df
#' contains columns site_id, longitude, and latitude.
#' @bird_dist_cutoff_m integer indicating the bird distance cutoff used to filter the
#' site-to-reach matches.
#' @param crosswalk data frame containing columns PRMS_segid, comid_seg, and comid_cat;
#' comid_seg and comid_cat are vectors of character strings representing the NHDPlusV2
#' COMIDs that intersect or drain to the NHM network, respectively. The crosswalk table
#' is built in USGS-R/drb-network-prep: https://github.com/USGS-R/drb-network-prep.
#' @param ref_gages sf object representing the ref-gages dataset, downloaded from
#' https://github.com/internetofwater/ref_gages. ref_gages should contain the
#' columns provider_id, name, provider, nhdpv2_COMID.
#'
#' @return returns a data frame containing the original sites data frame, with additional
#' flag attributes that can be used to QA/QC site-to-reach matches.
#'
#'
flag_seg_matches <- function(sites_df, bird_dist_cutoff_m, crosswalk, ref_gages){

  # Subset crosswalk table to include the NHDPlusv2 COMIDs that intersect the
  # NHM network
  crosswalk_segs <- crosswalk %>%
    select(PRMS_segid, comid_seg) %>%
    tidyr::separate_rows(comid_seg,sep=";") %>%
    rename(comid = comid_seg)

  # Subset crosswalk table to include all tributary NHDPlusV2 COMIDs in the DRB
  crosswalk_all_tribs <- crosswalk %>%
    select(PRMS_segid, comid_cat) %>%
    tidyr::separate_rows(comid_cat,sep=";") %>%
    rename(comid = comid_cat)

  # Append metadata from NHDPlus and NWIS to sites data frame; run time varies
  # depending on the download speeds in the nhdplusTools flowline fetch step and
  # the number of comids we're fetching.
  sites_df_w_metadata <- append_seg_match_metadata(sites_df,
                                                    bird_dist_cutoff_m = bird_dist_cutoff_m,
                                                    crosswalk = crosswalk_all_tribs,
                                                    ref_gages = ref_gages)

  # Add flags based on QC criteria for site-to-reach matching
  sites_df_w_flags <- sites_df_w_metadata %>%
    mutate(seg_match_flag_NHD = if_else((!comid_nearest_match %in% crosswalk_segs$comid),
                                        "Nearest NHDv2 reach not on NHM network", NA_character_),
           seg_match_flag_area = if_else(area_diff_km2 > 50,
                                         "Matched segment upstream area differs from NWIS area by >50 km2", NA_character_),
           seg_match_refgages_na = if_else((!is.na(provider_refgages) & is.na(nhdpv2_COMID_refgages)),
                                        "Site is in ref-gages but COMID is NA", NA_character_),
           seg_match_refgages = if_else((!is.na(nhdpv2_COMID_refgages) & (!nhdpv2_COMID_refgages %in% crosswalk_segs$comid)),
                                        "Site is in ref-gages but ref-gages COMID not on NHM network", NA_character_),
           seg_match_name = if_else(grepl("trib", name_refgages, ignore.case = TRUE),
                                    "Site name suggests trib but site is matched to mainstem", NA_character_))

  # Summarize the individual seg_match QC flags
  sites_df_w_flags_out <- sites_df_w_flags %>%
    # tally the number of seg_match_flags for each site (integer between 0 - 5)
    mutate(seg_match_flags_count = rowSums(!is.na(select(.,seg_match_flag_NHD:seg_match_name)))) %>%
    # concatenate the individual seg_match QC flags and format columns
    tidyr::unite("seg_match_flags", seg_match_flag_NHD:seg_match_name, na.rm = TRUE, sep = ';') %>%
    mutate(seg_match_flags = if_else(seg_match_flags == "", NA_character_, seg_match_flags)) %>%
    relocate("seg_match_flags",.after = last_col())

  return(sites_df_w_flags_out)

}



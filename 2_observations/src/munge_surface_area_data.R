read_monthly_surface_area_data <- function(res_sf_dir) {

  # check if data exists
  # if not, throw error indicating how to download
  if (!file.exists("2_observations/in/009130")) {
    stop("You must download reservoir monthly shapefile folder (0) data from:
         \nhttp://umnlcc.cs.umn.edu/realsat
         \nunzip the folder,
         \nthen locate folder '009130' and unzip the folder into '2_observations/in'.")
  }

  if (!file.exists("2_observations/in/573567")) {
    stop("You must download reservoir monthly shapefile folder (7) data from:
         \nhttp://umnlcc.cs.umn.edu/realsat
         \nunzip the folder,
         \nthen locate folder '573567' and unzip the folder into '2_observations/in'.")
  }

  # point to the data directory
  # getting list of shape files in this directory
  # getting the directory for cannonsville reservoir shape file.
  cannonsville_sf_dir <- list.files("2_observations/in/573567",
                                    pattern = ".shp$", full.names = TRUE)
  # getting the directory for pepacton reservoir shape file.
  pepacton_sf_dir <- list.files("2_observations/in/009130",
                                pattern = ".shp$", full.names = TRUE)

  res_sf_dir <- list(cannonsville_sf_dir, pepacton_sf_dir)

  return(res_sf_dir)
}


#### Function used to retrieve reservoirs surface area data #######

# Read in reservoirs monthly surface area data, munge them, and combine them.

# ReaLSAT base shapefile contains the reference shapefile of all the reservoirs.
# identify the reservoir's ID we interested in and
# download the monthly shape file that contains that shapefile
# Pull Pepacton (009130) and Cannsonville (573567)

combine_reaLSAT_reservoir_data <- function(cannonsville_sf_in, pepacton_sf_in,
                                           out_file){

  # read reservoirs shape file and convert it to dataframe.
  cannonsville_reaLSAT_data <- data.frame(read_sf(cannonsville_sf_in))
  pepacton_reaLSAT_data <- data.frame(read_sf(pepacton_sf_in))

  # extract columns of interest: reservoir id, month, year, and surface area
  # create reservoir column to provide reservoir name.
  cannonsvill_surf_area_data <- cannonsville_reaLSAT_data %>%
    select(id, month, year, area) %>%
    mutate(reservoir = 'Cannonsville')

  pepacton_surf_area_data <- pepacton_reaLSAT_data %>%
    select(id, month, year, area) %>%
    mutate(reservoir = 'Pepacton')

  # combine reservoirs munged data and save it to csv file.
  monthly_reservoir_data <- bind_rows(cannonsvill_surf_area_data, pepacton_surf_area_data)
  out <- readr:: write_csv(monthly_reservoir_data, path = out_file)
  return(out)

}

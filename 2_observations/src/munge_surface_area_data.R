read_monthly_surface_area_data <- function(res_sf_dir) {
  res_id <- c("009130", "573567")
  for (i in res_id){
    # Check if data exists,
    # if not, throw error indicating how to download the data.
    if (!file.exists(paste("2_observations/in/realsat/", i, sep=''))) {
      stop(paste("You must download reservoir monthly shapefile folder (0 or 7) data from:",
                 "http://umnlcc.cs.umn.edu/realsat, the corresponding shapefile folder is numbered according to the last digit of the ID. The ID can be found in the Base Shapefile.",
                 "unzip the folder",
                 "then locate folder '009130 or 573567' and unzip the folder into '2_observations/in/realsat'", sep='\n '))
    }
    # Point to the data directory.
    # Getting list of shape files in "2_observations/in/realsat" directory:
    res_sf_dir <- list(list.files(paste("2_observations/in/realsat/", i, sep=''),
                                  pattern = ".shp$", full.names = TRUE))

    return(res_sf_dir)
  }
}

#### Function used to retrieve reservoirs surface area data #######

# Read in reservoirs monthly surface area data, munge them, and combine them.

# realsat base shapefile contains the reference shapefile of all the reservoirs.
# Identify the reservoir's ID we interested in: The ID can be found in the Base Shapefile.
# Download the monthly shape file that contains that shapefile.
# Pull Pepacton (009130) and Cannsonville (573567).

combine_realsat_reservoir_data <- function(cannonsville_sf_in, pepacton_sf_in,
                                           out_file){

  # read reservoirs shape file and convert it to dataframe.
  cannonsville_realsat_data <- data.frame(read_sf(cannonsville_sf_in))
  pepacton_realsat_data <- data.frame(read_sf(pepacton_sf_in))

  # extract columns of interest: reservoir id, month, year, and surface area
  # create reservoir column to provide reservoir name.
  cannonsvill_surf_area_data <- cannonsville_realsat_data %>%
    select(id, month, year, area) %>%
    mutate(reservoir = 'Cannonsville')

  pepacton_surf_area_data <- pepacton_realsat_data %>%
    select(id, month, year, area) %>%
    mutate(reservoir = 'Pepacton')

  # combine reservoirs munged data and save it to csv file.
  monthly_reservoir_data <- bind_rows(cannonsvill_surf_area_data, pepacton_surf_area_data)
  out <- readr:: write_csv(monthly_reservoir_data, path = out_file)
  return(out)

}

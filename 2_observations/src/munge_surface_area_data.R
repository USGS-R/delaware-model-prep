#### Function used to retrieve reservoirs surface area data #######

# Read in reservoirs monthly surface area data, munge them, and combine them.

# realsat base shapefile contains the reference shapefile of all the reservoirs.
# Identify the reservoir's ID we interested in: The ID can be found in the Base Shapefile.
# The corresponding shapefile folder is numbered according to the last digit of the ID.
# Download the monthly shape file that contains that shapefile.
# Pull Pepacton (009130) can be retrieved from monthly shape file 0.
# And Cannsonville (573567) can be retrieved from monthly shape file 7.

retrieve_realsat_reservoir_data <- function(reservoir_ids, out_ind){
  # Entering reservoirs ids. then looping through reservoir ids one at a time.
  # Check if data file path exists,
  # If not, throw error indicating how to download the data.
  # If file path exists, it will be saved to reservoir_shapefiles.

  reservoir_shapefiles <- purrr::map(reservoir_ids, function(reservoir_id) {
    tryCatch(exp = read_sf(paste("2_observations/in/realsat/", reservoir_id, '/' ,
                                 reservoir_id, '.shp', sep='')),
             error = function(cond) {
               stop(paste("You must download reservoir monthly shapefile data ", reservoir_id, " from: ", sep = ''),
                    paste("http://umnlcc.cs.umn.edu/realsat",
                          "The shapefile data is located in a folder that is numbered according to the last digit of the ID.
                          E.g. shapefile (573567) is located in monthly shapefile '7'.",
                          "The ID can be found in the Base Shapefile.",
                          "unzip the folder",
                          "then locate the appropriate folder using the ID and unzip the folder into '2_observations/in/realsat'",
                          sep='\n '))
             }
    )
    # map function takes the file path and adds it to the reservoir_shapefiles (file path)
    list.files(paste("2_observations/in/realsat/", reservoir_id, sep=''),
               pattern = ".shp$", full.names = TRUE)

  })
  # using the reservoir_shapefiles (shape file paths).
  # read reservoirs shape file and convert it to dataframe.
  # convert area (in units of LANDSAT 30-m pixels) to m2
  # Extract columns of interest: reservoir id, month, year, and surface area.
  # Create reservoir column to provide the associated reservoir's name.
  raw_monthly_reservoir_data <- purrr::map_df(reservoir_shapefiles,
                                          function(reservoir_shapefile) {
                                            data.frame(read_sf(reservoir_shapefile)) %>%
                                              mutate(area_m2 = area*900) %>%
                                              select(id, month, year, area_m2)
                                          },
                                          .id = "reservoir") # the reservoir column will be made using the names of the listed of path file.

  readr:: write_csv(raw_monthly_reservoir_data, as_data_file(out_ind))
  gd_put(out_ind)
}

##### Function to combine realsat reservoir surface area data for each reservoir #####
combine_realsat_reservoir_data <- function(in_ind, out_ind) {
  raw_monthly_reservoir_data <- readr::read_csv(sc_retrieve(in_ind, 'getters.yml'), col_types='cdddd')

  # Add values for each year-month for each reservoir
  # convert year month columns to date, with 15th as day of month
  combined_monthly_reservoir_data <- raw_monthly_reservoir_data %>%
      group_by(reservoir, month, year) %>%
      summarize(area_m2 = sum(area_m2)) %>%
      ungroup() %>%
      mutate(day = 15) %>%
      mutate(date = as.Date(paste(year, month, day, sep="-"), "%Y-%m-%d")) %>%
      select(reservoir, date, area_m2) %>%
      arrange(reservoir, date)

  write_csv(combined_monthly_reservoir_data, as_data_file(out_ind))
  gd_put(out_ind)
}

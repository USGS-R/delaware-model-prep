
combine_realsat_reservoir_data <- function(reservoir_ids, out_file){
  # Check if data exists,
  # if not, throw error indicating how to download the data.
  reservoir_shapefiles <- c()
  for (i in reservoir_ids){
    tryCatch(exp = read_sf(paste("2_observations/in/realsat/", i, '/' , i, '.shp', sep='')),
             error = function(cond) {
               stop(paste("You must download reservoir monthly shapefile folders data from: ", i, sep = ''),
                    paste("http://umnlcc.cs.umn.edu/realsat",
                          "The corresponding shapefile folder is numbered according to the last digit of the ID. The ID can be found in the Base Shapefile.",
                          "unzip the folders",
                          "then locate the approapaite folders and unzip the folders into '2_observations/in/realsat'", sep='\n '))
               # Choose a return value in case of error
               return(NA)
             }
    )
    file_path <- list.files(paste("2_observations/in/realsat/", i, sep=''),
                            pattern = ".shp$", full.names = TRUE)

    reservoir_shapefiles <- append(reservoir_shapefiles, file_path)
  }

  monthly_reservoir_data <- purrr::map_df(read_sf(reservoir_shapefiles),
                                          (select), c(id, month, year, area) ,
                                          .id = names(realsat_ids))

  readr:: write_csv(monthly_reservoir_data, path = out_file)
  return(out_file)

}

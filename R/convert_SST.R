#' Convert SST Data
#'
#' @description This function converts SST  data from a given file into a structured format. It extracts information such as start year, end year, duration, longitude, latitude, and SST values from the data file. The function then creates a data frame with the extracted information and returns it along with the latitude-longitude data frame.
#'
#' @param datafile A data file containing SST data.
#' @param data_from The row index from where the SST data starts in the data file. Default is 5.
#'
#' @return A list containing the converted data frame and the latitude-longitude data frame.
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming the datafile is "sst_data.csv" and the SST data starts from row 5
#' #converted_data <- convert_SST("sst_data.csv", data_from = 5)
#' 
#' # Access the converted data frame and latitude-longitude data frame
#' #my_data <- converted_data[[1]]
#' #lat_lon_df <- converted_data[[2]]
#' 
#' # Print the converted data frame
#  #print(my_data)
# 
# # Plot the latitude-longitude data
#  #coordinates(lat_lon_df) <- ~longitude + latitude
#  #plot(lat_lon_df, col = my_data$SST_value, pch = 16, main = "SST Data")

#' @import sp
#' @import raster
convert_SST <- function(datafile, data_from = 5) {
  # Extract start year and end year
  start_year <- get_years_from_data(datafile)[1]
  end_year <- get_years_from_data(datafile)[length(get_years_from_data(datafile))]
  
  # Extract duration, longitude, and latitude
  duration <- get_years_from_data(datafile)
  lon <- get_lon_from_data(datafile)
  lat <- get_lat_from_data(datafile)
  
  # Create latitude-longitude dataframe
  lat_lon_df <- lat_lon_dataframe(datafile)
  
  period <- rep(get_years_from_data(datafile), each = (length(lat) * length(lon)))
  SST_value <- c()
  
  for (k in duration) {
    year <- matrix(NA, nrow = length(lat), ncol = length(lon))
    for (i in 1:length(lat)) {
      for (j in 1:length(lon)) {
        dat <- as.numeric(as.character(datafile[data_from + i, j + 1]))
        year[i, j] <- dat
        j = j + 1
      }
      i = i + 1
    }
    year <- as.data.frame(t(year))
    year <- stack(year)
    data_from = data_from + length(lat) + 2
    g <- as.numeric(year$values)
    SST_value <- append(SST_value, g)
  }
  
  my_data <- cbind(period, lat_lon_df, SST_value)
  return(list(my_data, lat_lon_df))
}


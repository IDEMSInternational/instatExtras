#' Create a dataframe with latitude and longitude information from a data file.
#' 
#' @description
#' This function creates a data frame with latitude and longitude information from a data file.
#' 
#' @param datafile The path or name of the data file.
#'
#' @return A data frame containing latitude, longitude, and station information.
#' 
#' @export
#'
#' @examples
#' # Create a latitude-longitude data frame
#' # datafile <- "data.csv"
#' # lat_lon_dataframe(datafile) 
#' 
lat_lon_dataframe <- function(datafile){
  latitude  <- get_lat_from_data(datafile)
  longitude <- get_lon_from_data(datafile)
  lat <- rep(latitude, each = length(longitude))
  lon <- rep(longitude, length(latitude))
  lat_lon <- as.data.frame(cbind(lat, lon))
  station <- c()
  for (j in 1:nrow(lat_lon)){
    if(lat_lon[j,1]>=0){
      station = append(station, paste(paste("latN", lat_lon[j,1], sep = ""), paste("lon", lat_lon[j,2], sep = ""), sep = "_"))
    } else{
      station = append(station, paste(paste("latS", abs(lat_lon[j,1]), sep = ""), paste("lon", lat_lon[j,2], sep = ""), sep = "_"))
    }
  }
  return(cbind(lat_lon,station))
}
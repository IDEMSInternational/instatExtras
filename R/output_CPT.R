#' Generate CPT data for visualization
#'
#' @description
#' This function generates a CPT (Color Palette Table) data frame for visualization purposes. It prepares the data by rearranging and merging the necessary variables and information. The resulting CPT data frame can be used for visualizing spatial data using CPT-based software or libraries.
#' 
#' @param data The input data frame containing the raw data.
#' @param lat_lon_data The data frame containing latitude and longitude information.
#' @param station_latlondata The column name representing the station in the lat_lon_data data frame.
#' @param latitude The column name representing latitude in the data frame.
#' @param longitude The column name representing longitude in the data frame.
#' @param station The column name representing the station in the data frame.
#' @param year The column name representing the year in the data frame.
#' @param element The column name representing the element in the data frame.
#' @param long.data If TRUE, the data frame contains all the required variables. If FALSE, the data frame contains multiple years stacked as columns.
#' @param na_code The code to be used for missing values. Default is -999.
#'
#' @return The CPT data frame for visualization.
#'  
#' @export
#'
#' @examples
#' # Example usage
#' # data <- read.csv("data.csv")
#' # lat_lon_data <- read.csv("lat_lon_data.csv")
#' # output_CPT(data, lat_lon_data, "station_latlon", "latitude", "longitude", "station", "year", "element")
#'
#' # Example usage with additional parameters
#' # output_CPT(data, lat_lon_data, "station_latlon", "latitude", "longitude", "station", "year", "element", long.data = FALSE, na_code = -1)
#' 
output_CPT <- function(data, lat_lon_data, station_latlondata, latitude, longitude, station, year, element, long.data = TRUE, na_code = -999) {
  
  if(missing(data)) stop("data should be provided")
  if(missing(station)) stop("station must be provided")
  if(missing(year) ||  missing(latitude) || missing(longitude)) stop("year, latitude and longiude must be provided")
  
  station_label <- "STN"
  lat_lon_labels <- c("LAT", "LON")
  
  if(missing(lat_lon_data)) {
    if(long.data) {
      data <- data %>% dplyr::select(!!! dplyr::quos(station, year, element, latitude, longitude))
      names(data)[1] <- "station"
      names(data)[2] <- "year"
      names(data)[3] <- "element"
      names(data)[4] <- "latitude"
      names(data)[5] <- "longitude"
      
      data <- data %>% dplyr::filter(!is.na(station))
    }
    else stop("If all data is in one data frame then must have long.data = TRUE")
  }
  else {
    if(missing(station_latlondata)) stop("station must be provided for lat_lon_data")
    
    if(long.data) {
      yearly_data <- data %>% dplyr::select(!!! dplyr::quos(station, year, element))
      names(yearly_data)[1] <-  "station"
      names(yearly_data)[2] <-  "year"
      names(yearly_data)[3] <-  "element"
      
      lat_lon_data <- lat_lon_data %>% dplyr::select(!!! dplyr::quos(station_latlondata, latitude, longitude))
      names(lat_lon_data)[1] <- "station"
      names(lat_lon_data)[2] <- "latitude"
      names(lat_lon_data)[3] <- "longitude"
      
      data <- merge(yearly_data, lat_lon_data, by =  "station")
    }
    else {
      stations <- data.frame(data[station])
      year <- data_unstacked %>% dplyr::select(!!! dplyr::quos(year))
      data <- data.frame(year, stations)
      stacked_data <- reshape2::melt(data, id.vars=c("year"))
      names(stacked_data)[2] <-  "station"
      names(stacked_data)[3] <- "element"
      
      lat_lon_data <- lat_lon_data %>% dplyr::select(!!! dplyr::quos(station_latlondata, latitude, longitude))
      names(lat_lon_data)[1] <- "station"
      names(lat_lon_data)[2] <- "latitude"
      names(lat_lon_data)[3] <- "longitude"
      
      data <- merge(stacked_data, lat_lon_data, by = "station")
      
    }
  }
  
  unstacked_data <- data %>% dplyr::select(station, year, element) %>% tidyr::spread(key = station, value = element)
  names(unstacked_data)[1] <- station_label
  unstacked_data <- unstacked_data %>% dplyr::mutate_all(dplyr::funs(replace(., is.na(.), na_code)))
  
  lat_lon_data <- data %>% dplyr::group_by(station) %>% dplyr::summarise(latitude = min(latitude, na.rm = TRUE), longitude = min(longitude, na.rm = TRUE))
  if(anyNA(data$latitude) || anyNA(data$longitude)) warning("Missing values in latitude or longitude.")
  t_lat_lon_data <- t(lat_lon_data)
  station.names <- as.vector(t_lat_lon_data[1, ])
  t_lat_lon_data <- data.frame(t_lat_lon_data, stringsAsFactors = FALSE)
  t_lat_lon_data <- t_lat_lon_data %>% dplyr::slice(-1)
  names(t_lat_lon_data) <- station.names
  row.names(t_lat_lon_data) <- lat_lon_labels
  t_lat_lon_data <- tibble::rownames_to_column(t_lat_lon_data, station_label)
  
  cpt_data <- rbind(t_lat_lon_data, unstacked_data)
  return(cpt_data)
}

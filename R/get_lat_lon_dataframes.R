#' Get Longitude coordinates from Data
#' 
#' 
#' @description
#' This function takes a data file as input and extracts the longitude values from it. The longitude values should be present in the second column of the fifth row. The function removes any missing or non-numeric values and returns a vector of unique longitude values.
#' The function returns a vector of unique longitude values extracted from the data file. It removes any missing or non-numeric values using the `na.omit()` function and transposes the extracted values using the `t()` function. The function is also exported, making it available for use outside the current package or script.
#' 
#' @param datafile A data file containing longitude values. The longitude values should be present in the second column of the fifth row.
#'
#' @return A vector of unique longitude values extracted from the data file.
#' 
#' @export
#'
#' @examples
#' # Example data file: mydata.csv
#' # Get longitude values from the data file
#' # data_file <- read.csv("mydata.csv")
#' # get_lon_from_data(data_file)
#' 
get_lon_from_data <- function(datafile){
  return(stats::na.omit(as.numeric(unique(t(datafile[5,2:ncol(datafile)])))))
}

#' Get Latitude values from a Data file
#' 
#' @description
#' This function takes a data file as input and extracts the latitude values from it. The latitude values should be present in the first column of the data file, starting from the 5th row. The function removes any missing or non-numeric values and returns a vector of unique latitude values.
#' 
#' @param datafile A data file containing latitude values. The latitude values should be present in the first column, starting from the 5th row.
#'
#' @return A vector of unique latitude values extracted from the data file.
#' 
#' @export
#'
#' @examples
#' # Example data file: mydata.csv
#' # Get latitude values from the data file
#' # data_file <- read.csv("mydata.csv")
#' # get_lat_from_data(data_file)
#' 
get_lat_from_data <- function(datafile){
  return(unique(stats::na.omit(as.numeric(as.character(datafile[5:nrow(datafile),1])))))
}

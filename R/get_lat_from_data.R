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
#' Get Years from Data
#'
#' Extracts unique years from a data file.
#'
#' @param datafile A data file or data frame.
#'
#' @return A vector of unique years extracted from the data file.
#' 
#' @export
#'  @examples
#'get_years_from_data <- function(datafile) {
#return(na.omit(t(unique(datafile[3, 2:ncol(datafile)]))))
#}
#' # Sample data file
#' datafile <- data.frame(
#'   Name = c("John", "Alice", "Bob"),
#'   Age = c(25, 30, 35),
#'   "2019" = c(100, 150, 200),
#'   "2020" = c(200, 300, 400),
#'   "2021" = c(300, 450, 600)
#' )
#'
#' # Get years from data file
#' years <- get_years_from_data(datafile)
#' years
#'

get_years_from_data <- function(datafile) {
  return(na.omit(t(unique(datafile[3, 2:ncol(datafile)]))))
  }


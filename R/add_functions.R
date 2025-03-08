#'  Modify a path
#' 
#' @description: This function takes a path as input and returns a modified path by appending "data.nc" to it.
#' 
#' @param path A character string representing the file path.
#' 
#' @return: A character string representing the modified file path with "data.nc" appended to it.
#' 
#' @export
#' 
#' @examples
#' add_nc("my_folder/")  # Returns "my_folder/data.nc"
#' add_nc("/path/to/file.txt")  # Returns "/path/to/file.txtdata.nc"
#' add_nc("")  # Returns "data.nc"
#' 
add_nc <- function(path) {
  paste0(path, "data.nc")
}

#' add time range
#'
#' @description  This function generates a string representing a time range to be added to a given path.
#'
#' @param path The base path to which the time range will be added.
#' @param min_date The minimum date of the time range.
#' @param max_date The maximum date of the time range.
#' @param dim_t The dimension of time to be included in the range (default: "T").
#'
#' @return A string representing the path with the time range added.
#'
#' @export
#'
#' @examples
#' # Example 1: Generate a time range string with default dimension
#' #path <- "http://example.com/"
#' #min_date <- lubridate::ymd("2023-01-01")
#' #max_date <- lubridate::ymd("2023-12-31")
#' #t_range <- add_t_range(path, min_date, max_date)
#'
#'
add_t_range <- function(path, min_date, max_date, dim_t = "T") {
  paste0(
    path, dim_t, "/",
    "(", lubridate::day(min_date), "%20", lubridate::month(min_date, label = TRUE),
    "%20", lubridate::year(min_date), ")", "/",
    "(", lubridate::day(max_date), "%20", lubridate::month(max_date, label = TRUE),
    "%20", lubridate::year(max_date), ")", "/",
    "RANGEEDGES", "/"
  )
}

#' Add xy area range
#'
#' @description
#' A function that generates a string representing an XY area range to be added to a given path.
#'
#' @param path The base path to which the XY area range will be added.
#' @param min_lon The minimum longitude of the area range.
#' @param max_lon The maximum longitude of the area range.
#' @param min_lat The minimum latitude of the area range.
#' @param max_lat The maximum latitude of the area range.
#' @param dim_x The dimension for longitude (default: "X").
#' @param dim_y The dimension for latitude (default: "Y").
#'
#' @return A string representing the path with the XY area range added.
#'
#' @export
#'
#' @examples
#' #Example: Generate an XY area range string with custom dimensions
#' #path <- "http://example.com"
#' #min_lon <- -90
#' #max_lon <- -80
#' #min_lat <- 30
#' #max_lat <- 40
#' #xy_range <- add_xy_area_range(path, min_lon, max_lon, min_lat,
#' #                              max_lat,dim_x = "LON", dim_y = "LAT")
#' 
add_xy_area_range <- function(path, min_lon, max_lon, min_lat, max_lat, dim_x = "X", dim_y = "Y") {
  paste0(
    path, "/", dim_x, "/",
    "(", ifelse(min_lon < 0, paste0(abs(min_lon), "W"), paste0(min_lon, "E")), ")", "/",
    "(", ifelse(max_lon < 0, paste0(abs(max_lon), "W"), paste0(max_lon, "E")), ")", "/",
    "RANGEEDGES", "/",
    dim_y, "/",
    "(", ifelse(min_lat < 0, paste0(abs(min_lat), "S"), paste0(min_lat, "N")), ")", "/",
    "(", ifelse(max_lat < 0, paste0(abs(max_lat), "S"), paste0(max_lat, "N")), ")", "/",
    "RANGEEDGES", "/"
  )
}


##' add xy area range
#'
#' This function generates a file path for XY point range data based on the provided parameters.
#'
#' @param path (character) The base path where the data file will be stored.
#' @param min_lon (numeric) The minimum longitude value for the range.
#' @param min_lat (numeric) The minimum latitude value for the range.
#' @param dim_x (character) The name of the X dimension. (Default: "X")
#' @param dim_y (character) The name of the Y dimension. (Default: "Y")
#'
#' @return The generated file path for the XY point range data as a character string.
#' @export
#'
#' @examples
#' add_xy_point_range("data", -90, 30, "X", "Y")
#'
add_xy_point_range <- function(path, min_lon, min_lat, dim_x = "X", dim_y = "Y"){
  paste0(
    path, "/", dim_x, "/", "(",
    ifelse(min_lon < 0, paste0(abs(min_lon), "W"), paste0(min_lon, "E")),
    ")", "/", "VALUES", "/", dim_y, "/", "(",
    ifelse(min_lat < 0, paste0(abs(min_lat), "S"), paste0(min_lat, "N")),
    ")", "/", "VALUES", "/"
  )
}

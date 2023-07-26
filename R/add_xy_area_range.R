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
#' #xy_range <- add_xy_area_range(path, min_lon, max_lon, min_lat, max_lat,dim_x = "LON", dim_y = "LAT")
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

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
add_xy_point_range <- function(path, min_lon, min_lat, dim_x = "X", dim_y = "Y") {
  paste0(
    path, "/", dim_x, "/",
    "(", ifelse(min_lon < 0, paste0(abs(min_lon), "W"), paste0(min_lon, "E")), ")", "/",
    "VALUES", "/",
    dim_y, "/",
    "(", ifelse(min_lat < 0, paste0(abs(min_lat), "S"), paste0(min_lat, "N")), ")", "/",
    "VALUES", "/"
  )
}

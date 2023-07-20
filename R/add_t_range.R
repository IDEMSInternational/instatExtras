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

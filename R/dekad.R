#' Get the dekade component of a date-time object
#'
#' @description Convert a date or date-time object to a yearly dekade (10-day period)
#'
#' @param date A date-time object
#'
#' @return a numerical vector of dekade objects corresponding to date variable.
#' @export
#'
#' @examples 
#' dekade(as.Date("2020/12/25"))
#' dekade(as.Date("1999/01/01"))
dekade <- function(date) {
  d <- 3 * (lubridate::month(date)) - 2 + (lubridate::mday(date) > 10) + (lubridate::mday(date) > 20)
  return(d)
}
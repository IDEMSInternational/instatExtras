#' Get the 366-based day of the year of a date
#'
#'@description
#' #' \code{yday_366} returns an integer between 1 and 366 representing the day of the year number of \code{x}.
#' 
#' In contrast to \code{lubridate::yday}, \code{yday_366} considers the length
#' of all years to be 366 days. In non leap years, there is no day 60 (29
#' February) and 1 March is always day 61.
#'
#' This convention is often used for weather data and other applications. It's
#' advantage is that all days have the same day number regardless of the year
#' e.g. 31 December is always day 366, even in non leap years. This may be
#' desirable, for example, when comparing day of year numbers across years that
#' contain leap years and non leap years.
#' 
#' @param date A Date object
#'
#' @return An integer between 1 and 366 representing the day of the year number of \code{x}.
#' 
#' @export
#'
#' @examples
#' # yday_366(as.Date("1999-03-01"))
#' # yday_366(as.Date("2000-12-31"))
#' # yday_366(as.Date("2005-12-31"))
#' 
yday_366 <- function(date) {
  temp_doy <- lubridate::yday(date)
  temp_leap <- lubridate::leap_year(date)
  temp_doy[(!is.na(temp_doy)) & temp_doy > 59 & (!temp_leap)] <- 1 + temp_doy[(!is.na(temp_doy)) & temp_doy > 59 & (!temp_leap)]
  return(temp_doy)
}
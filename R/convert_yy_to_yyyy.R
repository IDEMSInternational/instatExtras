

#' 
#' Convert Two-Digit Year to Four-Digit Year
#'
#' @description This function converts a two-digit year to a four-digit year based on a given base year. It adds 2000 to the two-digit year if it is less than or equal to the base year, otherwise, it adds 1900 to the two-digit year.
#'
#' @param x A numeric vector of two-digit years.
#' @param base The base year used as a reference for the conversion.
#'
#' @return A numeric vector of four-digit years.
#' @export
#'
#' @examples
#' # Example usage:
#' # Convert two-digit years to four-digit years based on the base year 1990
#' years <- c(92, 98, 04, 88)
#' converted_years <- convert_yy_to_yyyy(years, base = 1990)
#' print(converted_years)
#' # Output: 1992 1998 2004 1988
convert_yy_to_yyyy <- function(x, base) {
  if (missing(base))
    stop("base year must be supplied")
  dplyr::if_else(x + 2000 <= base, x + 2000, x + 1900)
  
}
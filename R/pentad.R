#' Calculate the pentad for a given date
#' 
#' @description
#' This function calculates the pentad (5-day period) for a given date. It determines which pentad the date falls into based on the day of the month.
#' 
#' @param date The input date.
#'
#' @return The pentad number corresponding to the input date.
#' 
#' @export
#'
#' @examples
#' # Example usage
#' # pentad(as.Date("2023-07-17"))
#'  
pentad <- function(date){
  temp_pentad <- 6*(lubridate::month(date)) - 5 +
    (lubridate::mday(date) > 5) +
    (lubridate::mday(date) > 10) +
    (lubridate::mday(date) > 15) +
    (lubridate::mday(date) > 20) +
    (lubridate::mday(date) > 25)
  return(temp_pentad)	
}
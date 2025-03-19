#' Get the default number of significant figures
#' 
#' @description This function gets the default number of significant figures when given a numeric vector.
#'
#' @param data \code{numeric(1)} A numerical vector
#'
#' @return If the data is numeric, "3", otherwise NA.
#' @export
#'
#' @examples 
#' x <- 1:8
#' get_default_significant_figures(x)
get_default_significant_figures <- function(data) {
  default_digits <- getOption("digits")
  if(is.numeric(data) || is.complex(data)){
    return(default_digits)
  } else { 
    return(NA)
  }
}

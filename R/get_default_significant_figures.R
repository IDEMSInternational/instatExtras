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
#' x <- 1:10
#' get_default_significant_figures(x)
get_default_significant_figures <- function(data) {
  if(is.numeric(data)) return(3)
  else return(NA)  
}

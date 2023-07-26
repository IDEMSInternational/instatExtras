#' Checking Binary variable
#' 
#' @description
#' This function checks if the input is a binary variable. It determines whether the input is of logical, numeric, or factor type and checks if it meets the criteria for a binary variable.
#'  
#' @param x The input variable to be checked for binary nature.
#'
#' @return The function returns a logical value indicating whether the input is binary or not. It returns TRUE if the input is binary, and FALSE otherwise.
#' 
#' @export
#'
#' @examples
#' is.binary(TRUE)           # TRUE
#' is.binary(c(0, 1, 1, 0))  # TRUE
#' is.binary(factor(c("Yes", "No", "Yes", "Yes")))  # TRUE
#' is.binary(c(1, 2, 3, 4))  # FALSE
#'  
is.binary <- function(x) {
  if(is.logical(x)) return(TRUE)
  else if(is.numeric(x)) return(all(stats::na.omit(x) %in% c(1,0)))
  else if(is.factor(x)) return(nlevels(x) == 2)
  else return(FALSE)
}
#' Is NA variable
#'
#' Check if a variable consists entirely of NA values.
#'
#' @param x An object to be checked.
#'
#' @return TRUE if the variable consists entirely of NA values, FALSE otherwise.
#' 
#' @export
#'
#' @examples
#' is.NAvariable(c(NA, NA, NA))
#' # Output: TRUE
#' 
#' is.NAvariable(c(1, 2, 3))
#' # Output: FALSE
#' 
#' is.NAvariable(c(TRUE, FALSE, NA))
#' # Output: FALSE
#' 
#' is.NAvariable(NULL)
#' # Output: FALSE

is.NAvariable <- function(x){
  get_length_x <- length(x) == sum(is.na(x))
  return(isTRUE(get_length_x))
}


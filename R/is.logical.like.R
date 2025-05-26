#' Logical like
#'
#' @description  Check if an object is logical-like.
#'
#' @param x An object to be checked.
#'
#' @return TRUE if the object is logical-like, FALSE otherwise.
#' 
#' @export
#'
#' @examples
#' is.logical.like(TRUE) 
#' # Output: TRUE
#' 
#' is.logical.like(c(TRUE, FALSE)) 
#' # Output: TRUE
#' 
#' is.logical.like(1) 
#' # Output: TRUE
#' 
#' is.logical.like(c(0, 1)) 
#' # Output: TRUE
#' 
#' is.logical.like("TRUE") 
#' # Output: TRUE
#' 
#' is.logical.like(NULL) 
#' # Output: FALSE

is.logical.like <- function(x) {
  x_no_na <- stats::na.omit(x)
  
  if (is.logical(x)) {
    return(TRUE)
  } else if (is.numeric(x)) {
    return(all(x_no_na %in% c(0, 1)))
  } else if (is.character(x)) {
    return(all(x_no_na %in% c("0", "1", "TRUE", "FALSE")))
  } else if (is.factor(x)) {
    levs <- levels(x)
    return(identical(sort(levs), c("0", "1")) || identical(sort(levs), c("FALSE", "TRUE")))
  } else {
    return(FALSE)
  }
}
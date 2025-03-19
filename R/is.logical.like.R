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
#' # Output: FALSE
#' 
#' is.logical.like(NULL) 
#' # Output: FALSE

is.logical.like <- function(x) {
  if(is.logical(x)){
    return(TRUE)
  } else if(is.numeric(x)) {
    return(all(stats::na.omit(x) %in% c(1,0)))
  } else {
    return(FALSE)
  }
}
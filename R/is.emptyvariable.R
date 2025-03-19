#' Check if Variable is Empty
#'
#' @description  This function checks if a variable is empty, i.e., if it contains only empty values.
#'
#' @param x The variable to be checked.
#'
#' @return A logical value indicating whether the variable is empty or not.
#'
#' @export
#'
#' @examples
#' is.emptyvariable(c("", "abc", ""))
#' # [1] FALSE
#'
#' is.emptyvariable(c("", ""))
#' # [1] TRUE

is.emptyvariable <- function(x){
  get_length_x <- length(x) == sum(x == "")
  return(isTRUE(get_length_x))
}
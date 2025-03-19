#' Get column attributes
#'
#' @description  This function retrieves the attributes of a given object, excluding specified attributes to be dropped.
#'
#' @param x The object for which attributes need to be retrieved.
#' @param drop A character vector specifying the names of attributes to be dropped from the result. Default is c("class", "levels").
#'
#' @return A list of attributes of the object, excluding the specified attributes to be dropped.
#' @export
#'
#' @examples
#' # Example 1: Get attributes of a vector
#' vec <- c(1, 2, 3)
#' get_column_attributes(vec)
#'
#' # Output:
#' # $dim
#' # NULL
#'
get_column_attributes <- function(x, drop = c("class", "levels")) {
  tmp_attr <- attributes(x)
  tmp_attr <- tmp_attr[!names(tmp_attr) %in% drop]
  return(tmp_attr)
}
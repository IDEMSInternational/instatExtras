#' Compare Columns
#' Compare two columns and provide information about their values
#' @param x The first column to compare.
#' @param y The second column to compare.
#' @param use_unique If TRUE, remove duplicate values from both columns before comparison. Default is TRUE.
#' @param sort_values If TRUE, sort the values in both columns before comparison. Default is TRUE.
#' @param firstnotsecond If TRUE, display the values that appear in the first column but not in the second column. Default is TRUE.
#' @param secondnotfirst If TRUE, display the values that appear in the second column but not in the first column. Default is TRUE.
#' @param display_intersection If TRUE, display the values that appear in both columns. Default is FALSE.
#' @param display_union If TRUE, display the values that appear in either column. Default is FALSE.
#' @param display_values If TRUE and the columns contain the same values, display the values. Default is TRUE.
#'
#' @return None.
#' @export
#'
#' @examples
#' x <- 1:10
#' y <- 5:14
#' compare_columns(x, y)
#'
compare_columns <- function(x, y, use_unique = TRUE, sort_values = TRUE, firstnotsecond = TRUE, secondnotfirst = TRUE, display_intersection = FALSE, display_union = FALSE, display_values = TRUE) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  if(use_unique) {
    x <- unique(x)
    y <- unique(y)
  }
  if(sort_values) {
    x <- sort(x)
    y <- sort(y)
  }
  equal <- setequal(x, y)
  cat(paste0("Columns contain all the same values: ", equal, "\n \n"))
  if(equal) {
    if(display_values) cat(paste0("Values: ", paste0("'", x, "'", collapse = ", "), "\n \n"))
  }
  if(!equal) {
    cat("First column:", x_name, "\n \n")
    cat("Second column:", y_name, "\n \n")
    if(firstnotsecond) {
      cat("Values in first not in second: ")
      setd <- dplyr::setdiff(x, y)
      if(length(setd) != 0) cat(paste0("'", setd, "'", collapse = ", "))
      cat("\n \n")
    }
    if(secondnotfirst) {
      cat("Values in second not in first: ")
      setd <- dplyr::setdiff(y, x)
      if(length(setd) != 0) cat(paste0("'", setd, "'", collapse = ", "))
      cat("\n \n")
    }
    if(display_intersection) {
      cat("Intersection (Values that appear in both columns):")
      inter <- dplyr::intersect(x, y)
      if(length(inter) != 0) cat(paste0("'", inter, "'", collapse = ", "))
      cat("\n \n")
    }
    if(display_union) cat(paste0("Union (Values that appear in either column): ", paste0("'", dplyr::union(x, y), "'", collapse = ", ")))
  }
}
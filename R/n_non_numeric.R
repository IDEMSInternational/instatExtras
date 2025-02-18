#' Count Non-Numeric Elements in a Vector
#'
#' This function counts the number of elements in a vector that are not purely numeric.
#'
#' @param x A vector of any type.
#'
#' @return An integer indicating the number of non-numeric elements in the vector.
#'
#' @details The function first converts the input to a character vector. It then
#' compares the presence of `NA` values before and after attempting numeric conversion.
#' Elements that fail numeric conversion are considered non-numeric.
#'
#' @examples
#' x <- c("10", "abc", "5.5", "NaN", "NA", "42", "3e2")
#' n_non_numeric(x)  # Should count the non-numeric values
#'
#' @export
n_non_numeric <- function(x) {
  x <- as.character(x)
  sum(is.na(x) != is.na(suppressWarnings(as.numeric(x))))
}
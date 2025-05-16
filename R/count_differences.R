#' Count Differences Between Two Vectors
#'
#' Compares two vectors element-wise and returns the number of differing elements.
#' Treats \code{NA != value} as a difference and considers \code{NA == NA} as equal.
#'
#' @param x A vector.
#' @param y A vector of the same length and type as \code{x}.
#'
#' @return An integer: the number of differing values between \code{x} and \code{y}.
#' Returns \code{NA_integer_} if \code{x} and \code{y} have different types.
#' 
#' @examples
#' count_differences(c(1, 2, NA), c(1, 3, NA))     # 1 difference (2 vs 3)
#' count_differences(c("a", "b"), c("a", "c"))     # 1 difference
#' count_differences(c("a", "b"), c(1, 2))     # NA because different type
#' count_differences(c(TRUE, NA), c(TRUE, FALSE)) # 1 difference (NA vs FALSE)
#'
#' @export
count_differences <- function(x, y) {
  if (length(x) != length(y)) stop("Vectors must be the same length.")
  if (typeof(x) != typeof(y)) return(NA_integer_)
  sum(!(is.na(x) & is.na(y)) & (is.na(x) != is.na(y) | x != y), na.rm = TRUE)
}

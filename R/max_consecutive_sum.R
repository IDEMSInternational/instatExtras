#' Compute the Maximum Consecutive Sum
#'
#' This function calculates the maximum sum of consecutive elements in a numeric vector.
#'
#' @param x A numeric vector.
#'
#' @return A numeric value representing the maximum consecutive sum.
#' 
#' @details This function relies on `consecutive_sum()`, assuming it computes 
#' consecutive sums with an initial value of 0. The maximum of these sums is returned.
#'
#' @seealso \code{\link{consecutive_sum}}
#'
#' @examples
#' x <- c(1, -2, 3, 4, -1, 2, 1, -5, 4)
#' max_consecutive_sum(x)
#'
#' @export
max_consecutive_sum <- function(x) {
  max(consecutive_sum(x, initial_value = 0))
}
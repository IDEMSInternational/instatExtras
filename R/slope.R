#' Slope
#'
#' @description Calculate the slope of a linear regression model.
#'
#' @param y A numeric vector of response variable values.
#' @param x A numeric vector of predictor variable values.
#'
#' @return The slope of the linear regression model.
#'
#' @export
#'
#' @examples
#' y <- c(1, 2, 3, 4, 5)
#' x <- c(2, 4, 6, 8, 10)
#' slope(y, x)
slope <- function(y, x) {
  x <- as.numeric(x)
  mod <- stats::lm(y ~ x)
  return(mod$coefficients[2])
}
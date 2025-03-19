#' Find values in the top N
#'
#' @description
#' This function takes a vector of values and checks if each value is among the top N values based on the specified criteria. The criteria can include weighting the values and applying a custom aggregation function to calculate a weighted score. The function returns a logical vector indicating whether each value is in the top N.
#' 
#' @param x The vector of values to be checked.
#' @param n The number of top values to consider. Default is 10.
#' @param wt A vector of weights corresponding to the values. If provided, the values will be weighted before determining the top N.
#' @param fun The aggregation function to be applied to calculate a weighted score. Default is sum.
#'
#' @return A logical vector indicating whether each value is in the top N.
#' 
#' @export
#'
#' @examples
#' # Check if values are in the top 5
#' # in_top_n(x = c(10, 5, 7, 12, 3), n = 5)
#'
#' # Check if weighted values are in the top 3 based on the sum
#' # in_top_n(x = c(10, 5, 7, 12, 3), n = 3, wt = c(2, 1, 3, 4, 2), fun = sum)
#' 
#' 
in_top_n <- function(x, n = 10, wt, fun = sum) {
  dat <- data.frame(x = x)
  if(!missing(wt)) {
    dat$wt <- wt
    dat <- dat %>% 
      dplyr::group_by(x) %>%
      dplyr::summarise(fq = as.function(fun)(stats::na.omit(wt))) %>%
      dplyr::arrange(-fq)
  } else {
    dat <- dat %>%
      dplyr::count(x, sort = TRUE, name = "fq")
  }
  return(x %in% dat$x[1:n])
}
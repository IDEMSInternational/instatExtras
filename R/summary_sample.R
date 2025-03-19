#'Calculate summary sample
#'
#' A function that summarizes a sample from a vector.
#'
#' @param x A vector from which to draw a sample.
#' @param size The size of the sample to be drawn.
#' @param replace Logical indicating whether sampling should be done with replacement (default: FALSE).
#'
#' @return A summary of the sample drawn from the input vector.
#'
#' @export
#'
#' @examples
#' # Example 1: Draw a sample from a vector
#' data <- c(1, 2, 3, 4, 5)
#' sample <- summary_sample(x = data, size = )
#'
#' # Example 2: Draw a sample from a vector with replacement
#' data <- c("A", "B", "C", "D", "E")
#' sample <- summary_sample(x = data, size = 4, replace = TRUE)
summary_sample <- function(x, size, replace = FALSE){
  if(length(x) == 0) {
    return(NA)
  } else if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x = x, size = size, replace = replace))
  }
}

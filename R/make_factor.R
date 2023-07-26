#' Make factor
#'
#' Creates a factor variable from the input data, with optional specification of ordering.
#'
#' @param x The input data to be converted into a factor variable.
#' @param ordered Logical value indicating whether the resulting factor should be ordered or not.
#'
#' @return A factor variable generated from the input data. If the input data is already a factor,
#' and the ordered parameter is consistent with the existing ordering, the input data is returned as is.
#' Otherwise, the input data is converted into a factor variable with the specified ordering.
#'
#' @export
#'
#' @examples
#' # Create a factor from a numeric vector
#' #make_factor(c(1, 2,3,3,2,2,1,1,1, 3, 2), ordered = TRUE)
#' # Output: 1 2 3 2
#' # Levels: 1 < 2 < 3
#'
#' # Create a factor from a logical vector
#' #make_factor(c(TRUE, FALSE,FALSE,TRUE,TRUE, TRUE), ordered = FALSE)
#' # Output: TRUE FALSE TRUE
#' # Levels: FALSE TRUE
#'
#' # Create a factor from a character vector
#' #make_factor(c("apple", "banana", "apple", "orange"), ordered = FALSE)
#' # Output: apple banana apple orange
#' # Levels: apple banana orange
#'
#' # Create a factor from an unsupported data type
#' #make_factor(Sys.time(), ordered = FALSE)
#' # Output: Error: The input data type is not supported for factor creation.
make_factor <- function(x, ordered = is.ordered(x)) {
  if (is.factor(x)) {
    if (ordered != is.ordered(x)) {
      if (ordered) class(x) <- c("ordered", class(x))
      else class(x) <- class(x)[class(x) != "ordered"]
    }
    x
  } else if (is.numeric(x)) {
    factor(x, ordered = ordered)
  } else if (is.logical(x)) {
    factor(x, levels = c("FALSE", "TRUE"), ordered = ordered)
  } else if (is.character(x)) {
    factor(x, levels = unique(x), ordered = ordered)
  } else {
    stop("Error: The input data type is not supported for factor creation.")
  }
}
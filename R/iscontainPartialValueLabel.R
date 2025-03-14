#' Check for Partial Value Labels
#'
#' @description
#' Checks if the variable contains partial value labels (some values labeled, others not).
#'
#' @param x A variable to check for partial value labels.
#'
#' @return A logical value. Returns `TRUE` if the variable contains partial value labels, otherwise `FALSE`.
#'
#' @examples
#' # Example with partially labeled variable
#' #is.containPartialValueLabel(x)
#'
#' @export
is.containPartialValueLabel <- function(x) {
  if (is.containValueLabel(x)) {
    levelCounts <- table(x)
    return(!all(x[!is.na(x)] %in% attr(x, labels_label)) && sum(levelCounts == 0) == 0)
  } else {
    return(FALSE)
  }
}

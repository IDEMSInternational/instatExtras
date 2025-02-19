#' Not-in Operator
#'
#' This operator returns `TRUE` if the left-hand side value is not in the right-hand side vector.
#'
#' @param x A value or vector to check.
#' @param table A vector to check against.
#'
#' @return A logical vector indicating if `x` is not in `table`.
#'
#' @examples
#' 5 %notin% c(1, 2, 3, 4) # TRUE
#' "a" %notin% c("a", "b", "c") # FALSE
#'
#' @export
`%notin%` <- function(x, table) {
  !(x %in% table)
}
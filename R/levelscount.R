#' Check Number of Levels in a Factor
#'
#' This function checks if a factor variable has a specific number of levels.
#'
#' @param x The factor variable to be checked.
#' @param n The expected number of levels.
#'
#' @return A logical value indicating whether the factor variable has the specified number of levels.
#'
#' @export
#'
#' @examples
#' #' # Example 1: Validating a factor variable with the correct number of levels
#' #factor_var <- factor(c("A", "B", "C", "D"))
#' #is.levelscount(factor_var, 4)
#' # Expected output: TRUE
#'
#' # Example 2: Validating a factor variable with an incorrect number of levels
#' #factor_var <- factor(c("A", "B", "C", "D"))
#' #is.levelscount(factor_var, 3)
#' # Expected output: FALSE
#'
is.levelscount <- function(x, n){
  #return(isTRUE(sum(levels(x)) == n))
}


#'  Cancor coef
#' 
#' @description: This function takes an object as input and returns the "xcoef" and "ycoef" elements from the object.
#' 
#' @param object An object containing "xcoef" and "ycoef" elements.
#' 
#' @return: A subset of the object containing only the "xcoef" and "ycoef" elements.
#' 
#' @export
#' 
#' @examples
#'
#' data <- list(xcoef = c(0.5, 0.3, -0.2), ycoef = c(0.8, -0.4, 0.6))
#' cancor_coef(data)
#' # Returns:
#' # $xcoef
#' # [1] 0.5 0.3 -0.2
#' #
#' # $ycoef
#' # [1] 0.8 -0.4 0.6
cancor_coef <- function(object) {
  object[c("xcoef", "ycoef")]
}
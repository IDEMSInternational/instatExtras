#' Checking if an object has a variable label attached.
#' 
#' @description
#' This function checks if an object has a variable label attached to it. It determines whether the input object `x` has a non-empty variable label using the `sjlabelled::get_label()` function.
#'
#' @param x The object to be checked for the presence of a variable label.
#'
#' @return
#' The function returns a logical value indicating whether the input object contains a variable label. It returns TRUE if a non-empty variable label is found, and FALSE otherwise.
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:10, y = 11:20)
#' sjlabelled::set_label(df$x, "Age")
#' is.containVariableLabel(df$x)  
#'
#' vec <- c(1, 2, 3, 4, 5)
#' is.containVariableLabel(vec)  
#' 
is.containVariableLabel <- function(x){
  get_label_x <- sjlabelled::get_label(x) != ""
  return(isTRUE(get_label_x))
}

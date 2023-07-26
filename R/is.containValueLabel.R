#' Checking if an object contains a value label attribute
#' 
#' @description
#' This function checks if an object contains a value label attribute. It determines whether the input object has an attribute with the name "labels_label".
#'  
#' @param x The object to be checked for the presence of a value label attribute.
#'
#' @return
#' The function returns a logical value indicating whether the input object contains a value label attribute. It returns TRUE if the attribute is present, and FALSE otherwise.
#' 
#' @export
#'
#' @examples
#' #df <- data.frame(x = 1:10, y = 11:20)
#' #attributes(df)$labels_label <- "Value Labels"
#' #is.containValueLabel(df)  # TRUE
#'
#' #vec <- c(1, 2, 3, 4, 5)
#' #is.containValueLabel(vec)  # FALSE 
#' 
is.containValueLabel <- function(x){
  return(labels_label %in% names(attributes(x)))
}

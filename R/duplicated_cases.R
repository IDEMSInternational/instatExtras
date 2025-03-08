#' Duplicated Cases
#' 
#' @description
#' This function identifies duplicated cases in a column and assigns a unique identifier to each duplicate case. The function supports both numeric and non-numeric columns.
#' 
#' @param col_name A vector representing the column for which duplicated cases need to be identified.
#' @param ignore A vector of values to be ignored when identifying duplicated cases. These values will not be considered as duplicates.
#' @param tolerance A numeric value representing the tolerance level for numeric columns. If the absolute difference between two consecutive numeric values is within this tolerance, they are considered duplicates.
#' 
#' @return A vector of the same length as the input column, where each element represents a unique identifier for each case. Duplicated cases are assigned the same identifier.
#' 
#' @export
#'
#' @examples
#' #' # Example 1: Numeric column
#' col <- c(1, 2, 2.01, 3, 4, 4.005, 4.01, 4.02, 4.03)
#' duplicated_cases(col)
#' 
#' # Example 2: Non-numeric column
#' col <- c("A", "A", "B", "C", "C", "D", "E")
#' duplicated_cases(col) 
#' 
duplicated_cases <- function(col_name, ignore = NULL, tolerance=0.01) {
  col_name <- as.vector(col_name)
  col_data1 <- c(1, rep(NA, length(col_name) - 1))
  if(is.numeric(col_name)) {
    for(i in 2:length(col_name)) {
      if(!is.na(col_data1[i-1])) col_data1[i] <- ifelse((col_name[i] >= (col_name[i-1] - tolerance)) & (col_name[i] <= (col_name[i-1] + tolerance)) & !(col_name[i] %in% ignore), col_data1[i-1] + 1, 1)
      else col_data1[i] <- ifelse(col_name[i] %in% ignore, 1, 1) 
    }
  } else {
    for(i in 2:length(col_name)) {
      if(!is.na(col_data1[i-1])) col_data1[i] <- ifelse((col_name[i] == col_name[i-1]) & !(col_name[i] %in% ignore), col_data1[i-1] + 1, 1)
      else col_data1[i] <- ifelse(col_name[i] %in% ignore, 1, 1)
    }
  }
  return(col_data1)
}

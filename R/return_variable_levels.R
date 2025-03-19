#' Generate JavaScript Sorting Function for Categorical Variables
#'
#' This function identifies categorical variables (factors) in a given dataset 
#' and generates a JavaScript sorting function that specifies the sorting order 
#' for each categorical variable based on its levels.
#'
#' @param data A data frame containing categorical variables (factors).
#'
#' @return A character string containing a JavaScript function that defines 
#'   custom sorting orders for categorical variables.
#' @export
#' 
#' @details This function is for use in the `Pivot Tables` dialog
#' 
#' @examples
#' df <- data.frame(
#'   category1 = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
#'   category2 = factor(c("A", "B", "C"), levels = c("A", "B", "C"))
#' )
#' js_sort_function <- return_variable_levels(df)
#' cat(js_sort_function)
return_variable_levels <- function(data){
  # Identify categorical variables (factors)
  categorical_vars <- names(Filter(is.factor, data))
  
  # Generate sorting order for all categorical variables
  sorting_orders <- purrr::map_chr(.x = categorical_vars,
                                   .f = function(var) {
                                     levels_var <- stringr::str_flatten(
                                       string = paste0("\"", levels(data[[var]]), "\",")
                                     )
                                     paste0("if (attr === '", var, "') return sortAs([", levels_var, "]);")
                                   })
  # Construct the JavaScript sorting function
  relevel_variables <- paste0(
    "function(attr) {  
     var sortAs = $.pivotUtilities.sortAs;  
     ", paste(sorting_orders, collapse = " "), "  
     return null;
   }"
  )
  return(relevel_variables)
}
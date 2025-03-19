#' Generate the next default item name
#'
#' This function generates a new item name based on the provided prefix. It is useful when creating default item names or ensuring uniqueness in a set of item names.
#'
#' @param prefix A character string representing the prefix for the item name.
#' @param existing_names A vector of existing item names. Defaults to an empty vector.
#' @param include_index A logical value indicating whether to include an index number in the generated item name. Defaults to \code{FALSE}.
#' @param start_index An integer indicating the starting index for generating the item name. Defaults to \code{1}.
#'
#' @return A character string representing the generated item name.
#'
#' @export
#'
#' @examples
#' next_default_item("item", c("item1", "item2", "item3"))
#' 
#' next_default_item("item", c("item1", "item2", "item3"), include_index = TRUE, start_index = 5)
next_default_item = function(prefix, existing_names = c(), include_index = FALSE, start_index = 1) {
  if(!is.character(prefix)){
    stop("prefix must be of type character") 
  }
  if(!include_index) {
    if(!prefix %in% existing_names){
      return(prefix)
    }
  }
  
  item_name_exists = TRUE
  start_index = 1
  while(item_name_exists) {
    out = paste0(prefix,start_index)
    if(!out %in% existing_names) {
      item_name_exists = FALSE
    }
    start_index = start_index + 1
  }
  return(out)
}

#' Split items into groups
#' @description This function takes a vector of items and splits them into a specified number of groups. The number of items must be divisible by the number of groups, otherwise an error is thrown.
#' @param items  A vector of items to be split into groups.
#' @param num The number of groups to split the items into. 
#'
#' @return A list containing the groups of items. Each element of the list represents a group and contains a subset of the original items.
#' @export
#'
#' @examples
#'  items <- c("A", "B", "C", "D", "E", "F", "G", "H")
#' num_groups <- 2
#' split_items_in_groups(items, num_groups)
#'
#' # Output:
#' # [[1]]
#' # [1] "A" "B" "C" "D"
#' #
#' # [[2]]
#' # [1] "E" "F" "G" "H"
split_items_in_groups <- function(items, num) {
  if(length(items) %% num != 0){
    stop("The number of items must be divisible by the number of groups")
  }
  x <- split(items, rep(1:num, each = length(items)/num))
  return(x)
}
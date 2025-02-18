#' Get Quarter of the year Label
#' 
#' @description
#' This function returns a three-letter string representing a specific quarter in a year.
#' 
#' @param quarter The quarter number. Must be in the range of 1 to 4.
#' @param start_month The starting month of the year. Must be in the range of 1 to 12.
#'
#' @return A factor object containing three-letter strings representing the specified quarters.
#' 
#' @export
#'
#' @examples
#' # Get quarter label starting from January
#' # get_quarter_label(quarter = 1, start_month = 1)
#'
#' # Get quarter label starting from April
#' # get_quarter_label(quarter = 2, start_month = 4)
#'
#' # Get quarter label starting from October
#' # get_quarter_label(quarter = 4, start_month = 10)
#' 
get_quarter_label <-   function(quarter, start_month){
  if (!start_month %in% 1:12) stop(start_month, " is an invalid start month, must be in range of 1:12")
  if (!all(quarter %in% 1:4)) stop(quarter, " is an invalid quarter, must be in range of 1:4")
  mabb <- rep(substr(month.abb, 1, 1), times = 2)[start_month:(11 + start_month)]
  qtr <- sapply(quarter, function(x){start_pos <- 1 + ((x-1) * 3)
  paste(mabb[start_pos:(start_pos+2)], collapse = "")})
  return(factor(x = qtr, levels = unique(qtr)))
}
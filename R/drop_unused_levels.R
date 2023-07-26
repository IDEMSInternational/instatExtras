#' Drop unused levels
#' @description  Drops unused levels from specified columns in a data frame.
#'
#' @param data A data frame.
#' @param columns A character vector specifying the column names in `data` from which unused levels should be dropped.
#'
#' @return The modified data frame with unused levels dropped from the specified columns.
#' If a column is not a factor, it remains unchanged.
#' 
#' @export
#'
#' @examples
#' # Create a data frame
#' df <- data.frame(A = factor(c("apple", "banana", "apple", "orange")),
#'                  B = factor(c("red", "blue", "green", "red")),
#'                  C = c(1, 2, 3, 4))
#'
#' # Drop unused levels from column 'A'
#' drop_unused_levels(df, columns = "A")
#' # Output:
#' #       A    B C
#' # 1 apple  red 1
#' # 2 banana blue 2
#' # 3 apple green 3
#' # 4 orange  red 4
#'
#' # Drop unused levels from multiple columns
#' drop_unused_levels(df, columns = c("A", "B"))
#' # Output:
#' #       A    B C
#' # 1 apple  red 1
#' # 2 banana blue 2
#' # 3 apple green 3
#' # 4 orange  red 4
drop_unused_levels <- function(data, columns) {
  for (i in seq_along(columns)) {
    if (is.factor(data[[columns[i]]])) data[[columns[i]]] <- droplevels(data[[columns[i]]])
  }
  return(data)
}
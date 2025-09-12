#' Format gt Tables
#'
#' Add Description
#'
#' @param df A data frame containing the summary data.
#' @param ... Additional options to read into gt()
#' 
#' @return A `gt` table with formatted styling and an automatically generated title.
#' @export
format_gt_table <- function(df, ...) {
  df %>%
    gt::gt(...) %>%
    gt::cols_label(.list = setNames(colnames(df), colnames(df)))
}
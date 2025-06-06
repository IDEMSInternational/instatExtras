#' Generate Summary Tables
#'
#' This function creates a styled summary table using `gt`, formatted with
#' a title extracted from the `summary-variable` column. The table is designed 
#' to display summarized data while dropping the `summary-variable` column.
#'
#' @param table_data A data frame containing the summary data, including a `summary-variable` column.
#'
#' @return A `gt` table with formatted styling and an automatically generated title.
#' @export
generate_summary_tables <- function(table_data) {
  if ("summary-variable" %in% names(table_data)) {
    unique_summary <- unique(table_data$`summary-variable`)
    if (length(unique_summary) == 1){
      summary_title <- table_data$`summary-variable` %>%
        unique() %>%
        gsub("__", " ", .) 
      table_data <- dplyr::select(table_data, -`summary-variable`)
    } else {
      return(gt::gt(table_data))
    }
  } else {
    unique_summary <- unique(table_data$summary)
    unique_variable <- unique(table_data$variable)
    
    if (length(unique_summary) == 1 & length(unique_variable) == 1) {
      summary_title <- paste(unique_summary, unique_variable, collapse = " ")
      table_data <- dplyr::select(table_data, -c("summary", "variable"))
    } else if (length(unique_summary) == 1) {
      summary_title <- unique_summary
      table_data <- dplyr::select(table_data, -summary)
    } else if (length(unique_variable) == 1) {
      summary_title <- unique_variable
      table_data <- dplyr::select(table_data, -variable)
    } else {
      summary_title <- ""
    }
  }
  gt_table <- gt::gt(table_data)
  gt_table <- gt::tab_header(gt_table, title = summary_title)
  return(gt_table)
}

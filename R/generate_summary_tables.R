#' Generate Summary Tables
#'
#' This function creates a styled summary table using `gt`, formatted with
#' a title extracted from the `summary-variable` column. The table is designed 
#' to display summarized data while dropping the `summary-variable` column.
#'
#' @param table_data A data frame containing the summary data, including a `summary-variable` column.
#' @param variable A character string representing the main variable to summarize.
#' @param second_factor A character string representing the secondary factor for grouping.
#'
#' @return A `gt` table with formatted styling and an automatically generated title.
#' @export
generate_summary_tables <- function(table_data, variable, second_factor) {
  # Extract unique value from 'summary-variable' to use as title
  summary_title <- table_data$`summary-variable` %>%
    unique() %>%
    gsub("__", " ", .) 
  
  # Drop the summary-variable column
  table_data <- table_data %>%
    dplyr::select(-`summary-variable`)
  
  table_data <- table_data %>% gt::gt()
  
  # Return the styled gt table
  table_data %>%
    gt::tab_header(title = summary_title)
}

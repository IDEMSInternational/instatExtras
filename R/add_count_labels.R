#' Computes group counts for a specified variable and appends a formatted
#' factor label column (`.label_col`) containing each value alongside its
#' sample size, ordered by the original factor levels.
#'
#' @param data A data frame or tibble.
#' @param var The unquoted name of the column to count and create labels for.
#' @param na.rm Logical. Determines whether rows with `NA` values in `var` be removed before counting. Defaults to `TRUE`.
#'
#' @return A data frame with two additional columns:
#' \describe{
#'   \item{`<var>_total`}{Integer count of occurrences for each level of `var`.}
#'   \item{`.label_col`}{A factor combining the values of `var` and their
#'     counts in the format `"Value (n=X)"`, ordered by the original levels of `var`.}
#' }
#'
#' @export
#'


add_count_labels <- function(data, var, na.rm = TRUE) {
  var_str <- rlang::as_name(rlang::ensym(var))
  total_col <- paste0(var_str, "_total")
  
  # Remove rows where the target variable is NA
  if (na.rm) {
    data <- data %>% dplyr::filter(!is.na({{ var }}))
  }
  
  plot_data <- data %>%
    dplyr::add_count({{ var }}, name = total_col) %>%
    dplyr::mutate(
      .label_col = paste0(.data[[var_str]], " (n=", .data[[total_col]], ")"),
      .label_col = reorder(.label_col, as.numeric(factor(.data[[var_str]])))
    )
  
  return(plot_data)
}
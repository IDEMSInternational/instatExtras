#' Check and summarise data levels
#'
#' This function checks the structure of a `summarise_data_levels` object.
#' It verifies the presence of an ID level, ensures there are no duplicated levels,
#' and identifies if Tricot data is missing. Depending on the situation, it prints a
#' numeric code or a summary of dataset levels.
#'
#' @param x A `summarise_data_levels` object. This should be a data frame with at least
#'   `level` and `dataset` columns.
#'
#' @return returns:
#' \itemize{
#'   \item `"0"` if no ID level is found (need ID level data to proceed),
#'   \item `"1"` if there are multiple datasets at the same level (should only have one per level),
#'   \item `"2"` if Tricot data is not found (all levels are `"No marker columns found."`),
#'   \item A concatenated message of dataset names and their corresponding levels if no issues are found.
#' }
#'
#' @examples
#' # Example 1: No ID level found (prints "0")
#' data_no_id <- data.frame(variety = c("A", "B", "C"))
#' levels_no_id <- summarise_data_levels(list(data1 = data_no_id))
#' check_data_levels(levels_no_id)
#'
#' # Example 2: Duplicate levels found (prints "1")
#' data_id1 <- data.frame(id = 1:3)
#' data_id2 <- data.frame(id = 4:6)
#' levels_duplicate_id <- summarise_data_levels(list(data1 = data_id1, data2 = data_id2))
#' levels_duplicate_id$level[1] <- "id"
#' levels_duplicate_id$level[2] <- "id" # artificially duplicate the level
#' check_data_levels(levels_duplicate_id)
#'
#' # Example 3: Tricot data not found (all levels are "No marker columns found.") (prints "2")
#' data_empty <- data.frame(x = 1:3)
#' levels_no_marker <- summarise_data_levels(list(data1 = data_empty))
#' levels_no_marker$level <- "No marker columns found." # simulate no markers found
#' check_data_levels(levels_no_marker)
#'
#' # Example 4: Normal case, prints dataset and level
#' data_good <- data.frame(id = 1:3, variety = c("A", "B", "C"))
#' levels_good <- summarise_data_levels(list(data1 = data_good))
#' check_data_levels(levels_good)
#' @export
check_data_levels <- function(x) {
  output_data_levels_check <- x %>% dplyr::filter(level != "No marker columns found.")
  if (all(x$level == "No marker columns found.")) {
    return("2") # Tricot Data not found.
  } else if (!"id" %in% x$level) {
    return("0") # Need ID level data to proceed.
  } else if (length(unique(output_data_levels_check$level)) != length(output_data_levels_check$level)) {
    return("1") # Multiple data frames given at ID level. Should only have one data frame at each level.
  } else {
    return("3") #paste0(x$dataset, " level: ", x$level, collapse = "; "))
  }
}
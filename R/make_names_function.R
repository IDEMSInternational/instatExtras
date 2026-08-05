#' Create a `names_pattern` for `tidyr::pivot_longer()`
#'
#' Creates a regular expression suitable for the `names_pattern` argument of
#' [tidyr::pivot_longer()] from a vector of column names and the number of
#' groups represented by those columns.
#'
#' The columns in `items` are assumed to be ordered so that corresponding
#' variables occur in the same position within each group. The columns are
#' divided into `num` groups using [split_items_in_groups()]. The common prefix
#' of each group is used as the `.value` component, while the part of the
#' names following the prefix is used as the `variable` component.
#'
#' @param items A character vector of column names to be grouped.
#' @param num The number of groups into which `items` should be divided.
#'
#' @return A character string containing a regular expression for use with
#'   `tidyr::pivot_longer(names_pattern = ...)`.
#'
#' @export
#' @examples
#' make_names_pattern(
#'   c("X1", "X2", "X3", "X4",
#'     "Y1", "Y2", "Y3", "Y4"),
#'   num = 2
#' )
#' # "(X|Y)(1|2|3|4)"
#'
#' make_names_pattern(
#'   c("height_male", "height_female",
#'     "weight_male", "weight_female"),
#'   num = 2
#' )
#' # "(height_|weight_)(male|female)"
make_names_pattern <- function(items, num) {
  groups <- instatExtras::split_items_in_groups(items, num)
  
  # Find the longest common prefix of a vector of names
  common_prefix <- function(x) {
    if (length(x) == 1) {
      return(x)
    }
    
    chars <- strsplit(x, "", fixed = TRUE)
    n <- min(lengths(chars))
    
    i <- which(vapply(
      seq_len(n),
      function(i) length(unique(vapply(chars, `[`, character(1), i))) > 1,
      logical(1)
    ))[1]
    
    if (is.na(i)) {
      paste0(vapply(chars[[1]], identity, character(1)), collapse = "")
    } else {
      paste0(chars[[1]][seq_len(i - 1)], collapse = "")
    }
  }
  
  prefixes <- vapply(groups, common_prefix, character(1))
  
  variable_names <- substring(
    groups[[1]],
    nchar(prefixes[[1]]) + 1
  )
  
  paste0(
    "(",
    paste(prefixes, collapse = "|"),
    ")(",
    paste(variable_names, collapse = "|"),
    ")"
  )
}
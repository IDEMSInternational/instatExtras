#' Count Number of Rows with Differences Across Multiple Vectors
#'
#' Compares multiple vectors row-wise and returns the number of rows
#' where not all values are equal (treating NA != value).
#'
#' @param ... Any number of vectors of equal length.
#'
#' @return An integer: number of rows where not all values are equal.
#'
#' @export
#' 
#' @examples
#' count_differences(c(1, 2, NA), c(1, 3, NA), c(1, 2, NA))
#' count_differences(c("a", "b"), c("a", "b"), c("a", "c"))
#' count_differences(c(1, 7), c(1, 3), c(1, 2))
#' count_differences(c("a", "b"), c(1, 3), c(1, 2)) 
count_differences <- function(...) {
  cols <- list(...)
  
  if (length(cols) < 2) stop("Need at least two vectors.")
  lengths <- vapply(cols, length, integer(1))
  if (length(unique(lengths)) != 1) stop("All vectors must be same length.")
  
  types <- vapply(cols, typeof, character(1))
  if (length(unique(types)) != 1) return(NA_integer_)
  
  mat <- do.call(cbind, cols)
  
  sum(apply(mat, 1, function(row) {
    # If all NA, consider equal
    if (all(is.na(row))) return(FALSE)
    # Remove NAs and check if all remaining values are identical
    vals <- row[!is.na(row)]
    length(unique(vals)) > 1 || any(is.na(row) & !all(is.na(row)))
  }))
}

#' Detect Tricot Data Structure
#'
#' Identifies option columns, good/bad trait columns, and rank values within a tricot dataset based on naming conventions.
#'
#' @param data A data frame that may contain tricot trial data.
#' @param prefix Character vector of suffixes that indicate trait-data (default `NULL`)
#' @param good_suffixes Character vector of suffixes that indicate positively ranked traits (default: `"_pos"`, `"_best"`).
#' @param bad_suffixes Character vector of suffixes that indicate negatively ranked traits (default: `"_neg"`, `"_worst"`).
#' @param na_candidates Character vector of values to treat as missing (default includes `"Not observed"`).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{option_cols}{Vector of column names matching `_a`, `_b`, or `_c` pattern for options.}
#'   \item{ranks}{Unique ranking values detected (excluding `na_candidates`).}
#'   \item{trait_good_cols}{Suffixes of columns identified as good trait responses.}
#'   \item{trait_bad_cols}{Suffixes of columns identified as bad trait responses.}
#'   \item{na_candidates}{The `na_candidates` input, for reference.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   option_a = c("x", "y", "z"),
#'   colour_pos = c("A", "B", "C"),
#'   taste_neg = c("C", "B", "Not observed")
#' )
#' detect_tricot_structure(df)
#'
#' @export
detect_tricot_structure <- function(data,
                                    prefix = NULL,
                                    good_suffixes = c("_pos", "_best", "best"),
                                    bad_suffixes = c("_neg", "_worst", "worst"),
                                    na_candidates = c("Not observed", "Not scored", NA_character_)) {
  
  # 1. Option columns (ending in _a/_b/_c or similar)
  if (!is.null(prefix)) {
    # If a prefix is provided, use it
    option_cols <- names(data)[grepl(paste0("^", prefix, "_[abc]$"), names(data), ignore.case = TRUE)]
  } else {
    # Otherwise, collect all columns ending in _a/_b/_c
    option_cols <- names(data)[grepl("_[abc]$", names(data), ignore.case = TRUE)]
    
    # Extract just the base part before _a/_b/_c
    base_names <- sub("_[abc]$", "", option_cols, ignore.case = TRUE)
    
    # Count how many times each base name appears
    base_counts <- table(base_names)
    
    # If multiple sets (e.g., var_a, var_b, var_c), try to prioritise based on known prefixes
    if (length(base_counts) > 1) {
      likely_prefixes <- c("variety", "option")
      
      # Choose bases that start with likely prefix
      preferred_bases <- names(base_counts)[base_counts >= 3 & grepl(paste(likely_prefixes, collapse = "|"), names(base_counts), ignore.case = TRUE)]
      
      # If any preferred sets found, filter option_cols to just those
      if (length(preferred_bases) > 0) {
        option_cols <- option_cols[sub("_[abc]$", "", option_cols, ignore.case = TRUE) %in% preferred_bases]
      }
    }
  }

  
  # 2. Trait columns ending in _pos/_neg/_best/_worst
  good_matched <- purrr::map_dfr(names(data), function(col) {
    matched_suffix <- good_suffixes[endsWith(col, good_suffixes)]
    
    if (length(matched_suffix) == 1) {
      values <- unique(na.omit(as.character(data[[col]])))
      dplyr::tibble(
        column = col,
        suffix = matched_suffix,
        values = list(values)
      )
    } else {
      NULL
    }
  })
  
  bad_matched <- purrr::map_dfr(names(data), function(col) {
    matched_suffix <- bad_suffixes[endsWith(col, bad_suffixes)]
    
    if (length(matched_suffix) == 1) {
      values <- unique(na.omit(as.character(data[[col]])))
      dplyr::tibble(
        column = col,
        suffix = matched_suffix,
        values = list(values)
      )
    } else {
      NULL
    }
  })
  
  rank_values <- setdiff(unlist(good_matched$values), na_candidates)
  
  # Extract suffixes from option_cols (e.g., "a", "b", "c")
  suffix_order <- sub(".*_", "", option_cols)
  
  # Reorder ranks based on matching lowercase suffixes
  rank_values <- rank_values[match(toupper(suffix_order), rank_values)]
  
  list(
    option_cols = option_cols,
    ranks = rank_values,
    trait_good_cols = unique(good_matched$suffix),
    trait_bad_cols = unique(bad_matched$suffix),
    na_candidates = na_candidates
  )
}

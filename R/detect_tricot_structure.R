#' Detect Tricot Data Structure
#'
#' Identifies option columns, trait columns (good/bad), and rank values in a tricot dataset based on naming conventions.
#'
#' @param data A data frame containing tricot trial data.
#' @param variety_cols Optional character vector of column names for option sets (e.g., c("option_a", "option_b", "option_c")). Autodetected by suffix `_a`, `_b`, `_c` if `NULL`.
#' @param rank_values Character vector of possible rank values (e.g., `c("A", "B", "C")`). If `NULL`, inferred from observed trait values.
#' @param prefix Optional character string used to filter option columns by a common prefix before `_a`, `_b`, `_c` when autodetecting.
#' @param good_suffixes Character vector of suffixes indicating positively ranked traits (default: `c("_pos", "_best", "best")`).
#' @param bad_suffixes Character vector of suffixes indicating negatively ranked traits (default: `c("_neg", "_worst", "worst")`).
#' @param na_candidates Character vector of values to treat as missing (default: `c("Not observed", "Not scored", NA_character_)`).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{option_cols}{Character vector of detected or specified option columns.}
#'   \item{ranks}{Character vector of rank values used to order options.}
#'   \item{trait_good_cols}{Character vector of detected suffixes for good traits.}
#'   \item{trait_bad_cols}{Character vector of detected suffixes for bad traits.}
#'   \item{na_candidates}{The original `na_candidates` vector, for reference.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   id = 1:3,
#'   option_a   = c("x", "y", "z"),
#'   colour_pos = c("A", "B", "C"),
#'   taste_neg  = c("C", "B", "Not observed")
#' )
#' detect_tricot_structure(df)
#'
#' @export
detect_tricot_structure <- function(data,
                                    variety_cols = NULL,
                                    rank_values = c("A", "B", "C"),
                                    prefix = NULL,
                                    good_suffixes = c("_pos", "_best", "best"),
                                    bad_suffixes = c("_neg", "_worst", "worst"),
                                    na_candidates = c("Not observed", "Not scored", NA_character_)) {
  
  # Get the names of our variety/option columns
  # This can be specified or autodetected.
  if (is.null(variety_cols)){ # Autodetect (else variety_cols = a list of the columns)
    if (!is.null(prefix)) {
      # If a prefix is provided, use it
      variety_cols <- names(data)[grepl(paste0("^", prefix, "_[abc]$"), names(data), ignore.case = TRUE)]
    } else {
      # Otherwise, collect all columns ending in _a/_b/_c
      variety_cols <- names(data)[grepl("_[abc]$", names(data), ignore.case = TRUE)]
      
      # Extract just the base part before _a/_b/_c
      base_names <- sub("_[abc]$", "", variety_cols, ignore.case = TRUE)
      
      # Count how many times each base name appears
      base_counts <- table(base_names)
      
      # If multiple sets (e.g., var_a, var_b, var_c), try to prioritise based on known prefixes
      if (length(base_counts) > 1) {
        likely_prefixes <- c("variety", "option")
        
        # Choose bases that start with likely prefix
        preferred_bases <- names(base_counts)[base_counts >= 3 & grepl(paste(likely_prefixes, collapse = "|"), names(base_counts), ignore.case = TRUE)]
        
        # If any preferred sets found, filter variety_cols to just those
        if (length(preferred_bases) > 0) {
          variety_cols <- variety_cols[sub("_[abc]$", "", variety_cols, ignore.case = TRUE) %in% preferred_bases]
        }
      }
    }
  }

  
  # 2. Trait columns: Get the names of our traits
  # always ending in _pos/_neg/_best/_worst
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
  
  if (is.null(rank_values)){
    rank_values <- setdiff(unlist(good_matched$values), na_candidates)
    
    # Extract suffixes from variety_cols (e.g., "a", "b", "c")
    suffix_order <- sub(".*_", "", variety_cols)
    
    # Reorder ranks based on matching lowercase suffixes
    rank_values <- rank_values[match(toupper(suffix_order), rank_values)]
  }
  
  list(
    option_cols = variety_cols,
    ranks = rank_values,
    trait_good_cols = unique(good_matched$suffix),
    trait_bad_cols = unique(bad_matched$suffix),
    na_candidates = na_candidates
  )
}

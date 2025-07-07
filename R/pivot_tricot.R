#' Pivot Tricot Data
#'
#' Transforms Tricot (Triadic Comparison of Technologies) data into a tidy format,
#' assigning ranks to varieties based on trait evaluations.  Supports input either
#' as a single ID-level dataset (data) with option columns and trait indicators,
#' or as a plot–trait–level dataset (data_plot_trait) with explicit variety,
#' trait, and rank columns, or both.
#'
#' @param data A data frame containing ID-level Tricot data.  Must include an ID column
#'   (data_id_col) and three option columns (option_cols), plus trait indicator
#'   columns ending in trait_good or trait_bad, unless data_plot_trait is provided.
#' @param data_id_col Character string naming the ID column in data.  Default is "id".
#' @param data_trait_cols Optional character vector of specific trait-indicator columns
#'   in data to restrict scanning (overrides automatic selection by suffix).  Default NULL.
#' @param carry_cols Optional character vector of column names in data to carry along
#'   into the output (e.g. blocking factors, metadata).  Default NULL.
#' @param data_plot_trait A data frame in “plot–trait” format containing one row per
#'   (ID, variety, trait) with an associated rank_col.  Default NULL.
#' @param data_plot_trait_id_col Character string naming the ID column in
#'   data_plot_trait.  Default is "id".
#' @param variety_col Character string naming the variety column in data_plot_trait.
#' @param trait_col Character string naming the trait column in data_plot_trait.
#' @param rank_col Character string naming the rank column in data_plot_trait.
#' @param option_cols Character vector of length three naming the option columns in
#'   data (e.g. c("option_a", "option_b", "option_c")).  Default is
#'   c("option_a", "option_b", "option_c").
#' @param possible_ranks Character vector of labels corresponding to option_cols
#'   (e.g. c("A", "B", "C")).  Default is c("A", "B", "C").
#' @param trait_good Suffix (or full name) identifying columns in data where a
#'   variety is positively ranked for a trait (e.g. "_pos" or "best").  Default "_pos".
#' @param trait_bad Suffix (or full name) identifying columns in data where a
#'   variety is negatively ranked for a trait (e.g. "_neg" or "worst").  Default "_neg".
#' @param na_value Value used in data to indicate a missing or unobserved trait.
#'   Default is "Not observed".
#'
#' @return A tibble in tidy format with one row per (ID, trait, variety) combination,
#' containing at least the following columns:
#'   \describe{
#'     \item{<data_id_col>}{Identifier for each trial or plot (column name retained).}
#'     \item{dummy_variety}{Factor-coded option label corresponding to the variety (e.g. "A", "B", "C").}
#'     \item{trait}{Name of the trait being ranked (when data input used).}
#'     \item{rank}{Numeric or character rank assigned to the variety.}
#'     \item{variety}{Name of the variety or option.}
#'     \item{<carry_cols>}{Any additional columns specified via carry_cols, carried through from data.}
#'   }
#'   Additional trait-specific columns will appear if both data and
#'   data_plot_trait are provided.
#'
#' @export
pivot_tricot <- function(data = NULL,
                         data_id_col = "id",
                         data_trait_cols = NULL,
                         carry_cols = NULL,
                         data_plot_trait = NULL,
                         data_plot_trait_id_col = "id",
                         variety_col = NULL,
                         trait_col = NULL,
                         rank_col = NULL,
                         option_cols = c("option_a", "option_b", "option_c"),
                         possible_ranks = c("A", "B", "C"),
                         trait_good = "_pos",
                         trait_bad = "_neg",
                         na_value = "Not observed") {
  
  # Input checks
  if (is.null(data) && is.null(data_plot_trait)) {
    stop("At least one of data or data_plot_trait must be provided.")
  }
  if (!is.null(data) && is.null(data_id_col)) {
    stop("If data is provided, data_id_col must also be specified.")
  }
  if (!is.null(data_plot_trait)) {
    if (is.null(data_plot_trait_id_col) || is.null(variety_col) || is.null(trait_col) || is.null(rank_col)) {
      stop("If data_plot_trait is provided, then data_plot_trait_id_col, variety_col, trait_col, and rank_col must also be provided.")
    }
  }
  
  # Helpers for tidy evaluation
  id_data_sym <- rlang::sym(data_id_col)
  id_plot_sym <- rlang::sym(data_plot_trait_id_col)
  
  # Process data_plot_trait
  if (!is.null(data_plot_trait)) {
    data_options_a <- data_plot_trait %>%
      tidyr::pivot_wider(names_from   = dplyr::all_of(trait_col),
                         values_from = dplyr::all_of(rank_col))
  }
  
  # Process data
  if (!is.null(data)) {
    # Select trait indicator columns
    if (!is.null(data_trait_cols)) {
      matching_cols <- data %>% dplyr::select(dplyr::all_of(data_trait_cols))
    } else {
      matching_cols <- data %>% dplyr::select(dplyr::ends_with(trait_good), dplyr::ends_with(trait_bad))
    }
    
    # Map option labels to ranks
    label_map <- stats::setNames(option_cols, possible_ranks)
    
    # Pivot options to long format, retaining original ID column name
    data_options <- data %>%
      dplyr::mutate(!!id_data_sym := .data[[data_id_col]]) %>%
      tidyr::pivot_longer(cols       = dplyr::all_of(option_cols),
                          names_to   = "Label",
                          values_to  = "variable") %>%
      dplyr::mutate(Label = forcats::fct_recode(Label, !!!label_map))
    
    data_options_id <- data_options %>%
      dplyr::select(dplyr::all_of(c(data_id_col, carry_cols)),
                    variety       = variable,
                    dummy_variety = Label) %>%
      unique()
    
    if (ncol(matching_cols) > 0) {
      # Convert trait indicators into ranks
      data_options_b <- data %>%
        dplyr::mutate(!!id_data_sym := .data[[data_id_col]]) %>%
        tidyr::pivot_longer(cols      = dplyr::all_of(names(matching_cols)),
                            names_to  = "trait",
                            values_to = "variety") %>%
        dplyr::select(!!id_data_sym, trait, variety) %>%
        dplyr::mutate(rank = ifelse(grepl(trait_good, trait), 1, 3),
                      trait = sub("(_[^_]*)$", "", trait))
      
      # Handle middle ranks and reshape
      if (ncol(matching_cols) == 2) {
        missing_mid <- data_options_b %>%
          dplyr::group_by(!!id_data_sym) %>%
          dplyr::summarise(missing_var = setdiff(possible_ranks, variety), .groups = "drop") %>%
          dplyr::mutate(trait = "middle", rank = 2) %>%
          dplyr::rename(variety = missing_var)
        
        data_options_b <- dplyr::bind_rows(data_options_b, missing_mid) %>%
          dplyr::arrange(!!id_data_sym, rank) %>%
          dplyr::rename(dummy_variety = variety) %>%
          dplyr::full_join(data_options_id)
      } else {
        # General case for >=3 traits
        ranked <- data_options_b %>%
          dplyr::filter(!is.na(variety) & variety != na_value) %>%
          dplyr::count(!!id_data_sym, trait) %>%
          dplyr::filter(n == 2) %>%
          dplyr::mutate(rank = 2)
        
        data_options_b <- data_options_b %>%
          dplyr::full_join(ranked) %>%
          dplyr::group_by(!!id_data_sym, trait) %>%
          dplyr::mutate(variety = ifelse(is.na(variety), setdiff(possible_ranks, variety)[1], variety)) %>%
          dplyr::ungroup() %>%
          dplyr::rename(dummy_variety = variety) %>%
          dplyr::full_join(data_options_id) %>%
          dplyr::select(dplyr::all_of(c(data_id_col, carry_cols)), trait, rank, variety, dummy_variety) %>%
          tidyr::pivot_wider(names_from = trait, values_from = rank) %>%
          dplyr::filter(!is.na(variety))
      }
    } else {
      if (!is.null(data_plot_trait)) {
        data_options_a <- dplyr::full_join(data_options_a, data_options_id)
      } else {
        stop(paste0("No columns ending with ", trait_good, " or ", trait_bad, " found in data."))
      }
    }
  }
  
  # Combine data and plot-trait if both provided
  if (!is.null(data) && !is.null(data_plot_trait) && ncol(matching_cols) > 0) {
    message("Using both data and data_plot_trait")
    data_options <- dplyr::full_join(data_options_a, data_options_b)
    
    if (nrow(data_options_a) == nrow(data_options_b) && nrow(data_options) != nrow(data_options_a)) {
      bad_ids <- data_options %>%
        dplyr::group_by(!!id_data_sym) %>%
        dplyr::count() %>%
        dplyr::filter(n > length(option_cols)) %>%
        dplyr::pull(!!id_data_sym)
      warning("Repeating entries for IDs: ", paste(bad_ids, collapse = ", "))
    }
  } else if (!is.null(data_plot_trait)) {
    data_options <- data_options_a
  } else {
    data_options <- data_options_b
  }
  
  return(data_options)
}

#' Pivot Tricot Data
#'
#' This function transforms Tricot (Triadic Comparison of Technologies) data 
#' into a tidy format, assigning ranks based on trait evaluations.
#'
#' It supports input either as a single dataset (`data`) or as two datasets:
#' one with trait-wise rankings (`data_plot_trait`) and one with option labels and traits.
#'
#' The function assumes a tricot setup where three options (e.g. crops, varieties) are ranked per trait.
#' Rankings may be extracted from columns ending in a suffix such as `_pos` or `_neg`, or explicitly provided.
#'
#' @param data A data frame containing option columns and trait ranking indicators (e.g. columns ending in `_pos` or `_neg`).
#' @param data_id_col The name of the ID column in `data`. Default is `"id"`.
#' @param data_plot_trait A data frame containing ID, variety, trait, and rank columns.
#' @param data_plot_trait_id_col The name of the ID column in `data_plot_trait`. Default is `"id"`.
#' @param variety_col The name of the variety column in `data_plot_trait`.
#' @param trait_col The name of the trait column in `data_plot_trait`.
#' @param rank_col The name of the rank column in `data_plot_trait`.
#' @param option_cols A character vector of column names in `data` that store the option labels (e.g. `"option_a"`, `"option_b"`, `"option_c"`).
#' @param possible_ranks A character vector mapping to `option_cols`, indicating rank labels (e.g. `"A"`, `"B"`, `"C"`).
#' @param trait_good A suffix string (e.g. `"_pos"`) used to identify columns in `data` where a variety is "preferred" for a trait. Default is `"_pos"`.
#' @param trait_bad A suffix string (e.g. `"_neg"`) used to identify columns in `data` where a variety is "not preferred" for a trait. Default is `"_neg"`.
#' @param na_value A value used to indicate when a trait was not observed or is missing. Default is `"Not observed"`.
#'
#' @return A tibble with the transformed Tricot data, containing columns:
#'   - `id`: Identifier for each observation.
#'   - `dummy_variety`: (Optional). The dummy variable that variety is associated to.
#'   - `rank`: The rank assigned to each variety.
#'   - `variety`: The variety name.
#'   - And a set of trait variables
#'
#' @export
#'
#' @examples
#' library(gosset)
#' # Read in tricot data from the `gosset` library
#' cassava_pivot <- pivot_tricot(cassava)
#' 
#' # Example using nicabean data - if we have Plot-Trait Level data
#' data(nicabean)
#' nicabean_by_id_item_trait <- nicabean$trial
#' nicabean_by_id <- nicabean$covar 
#' nicabean_by_plot <- pivot_tricot(data_plot_trait = nicabean_by_id_item_trait, # B
#'                                        data_plot_trait_id_col = "id",
#'                                        variety_col = "item",
#'                                        trait_col = "trait",
#'                                        rank_col = "rank")
#' 
#' # Example using breadwheat data - if we have just ID-Level data 
#' data(breadwheat)
#' breadwheat_by_plot <- pivot_tricot(data = breadwheat,
#'                                          data_id_col = "participant_name",
#'                                          option_cols = c("variety_a", "variety_b", "variety_c"),
#'                                          trait_good = "best", trait_bad = "_worst")
#' 
#' # Example using nicabean data - if we have both data and data_plot_trait data
#' nicabean_by_id$Vigor_pos = "A"
#' nicabean_by_id$Vigor_neg = "C"
#' nicabean_by_plot_2 <- pivot_tricot(data = nicabean_by_id,
#'                                          data_id_col = "id",
#'                                          option_cols = c("variety_a", "variety_b", "variety_c"),
#'                                          data_plot_trait = nicabean_by_id_item_trait,
#'                                          data_plot_trait_id_col = "id",
#'                                          variety_col = "item",
#'                                          trait_col = "trait",
#'                                          rank_col = "rank")
pivot_tricot <- function(data = NULL, data_id_col = "id", data_plot_trait = NULL,
                         data_plot_trait_id_col = "id",
                         variety_col = NULL,
                         trait_col = NULL, rank_col = NULL,
                         option_cols = c("option_a", "option_b", "option_c"),
                         possible_ranks = c("A", "B", "C"), trait_good = "_pos", 
                         trait_bad = "_neg", na_value = "Not observed") {
  
  # Input checks
  if (is.null(data) & is.null(data_plot_trait)) {
    stop("At least one of `data` or `data_plot_trait` must be provided.")
  }
  
  if (!is.null(data) & is.null(data_id_col)) {
    stop("If `data` is provided, `data_id_col` must also be specified.")
  }
  
  if (!is.null(data_plot_trait)) {
    if (is.null(data_plot_trait_id_col) | is.null(variety_col) | is.null(trait_col) | is.null(rank_col)) {
      stop("If `data_plot_trait` is provided, then `data_plot_trait_id_col`, `variety_col`, `trait_col`, and `rank_col` must also be provided.")
    }
  }
  
  # Handle data_plot_trait if relevant columns are given
  if (!is.null(variety_col) & !is.null(trait_col) & !is.null(rank_col)) {

    data_options_a <- data_plot_trait %>%
      tidyr::pivot_wider(names_from = dplyr::all_of(trait_col),
                         values_from = dplyr::all_of(rank_col))
    
    lookup <- c(id = data_plot_trait_id_col, variety = variety_col)
    data_options_a <- dplyr::rename(data_options_a, dplyr::all_of(lookup))
  }
  
  # Handle data
  if (!is.null(data)) {
    matching_cols <- data %>% dplyr::select(dplyr::ends_with(trait_good), dplyr::ends_with(trait_bad))
    
    label_map <- stats::setNames(option_cols, possible_ranks)
    data_options <- data %>%
      dplyr::mutate(id = data[[data_id_col]]) %>% 
      tidyr::pivot_longer(cols = dplyr::all_of(option_cols), names_to = "Label", values_to = "variable") %>% 
      dplyr::mutate(Label = forcats::fct_recode(Label, !!!label_map))
    
    data_options_id <- data_options %>%
      dplyr::select(c(id, variety = Label, variable)) %>%
      unique()
    
    if (ncol(matching_cols) > 0) {
      data_options <- data %>%
        dplyr::mutate(id = data[[data_id_col]]) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(c(dplyr::ends_with(trait_good), 
                                                   dplyr::ends_with(trait_bad))),
                            names_to = "trait", values_to = "variety") %>%
        dplyr::select(c(id, trait, variety)) %>%
        dplyr::mutate(rank = ifelse(grepl(trait_good, trait), 1, 3)) %>%
        dplyr::mutate(trait = sub("(_[^_]*)$", "", trait))
      
      if (ncol(matching_cols) == 2){ # here, we have just "best" and "worst"
        data_options_b <- data_options %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(missing_var = setdiff(possible_ranks, variety), .groups = "drop") %>%
          dplyr::mutate(trait = "middle", rank = 2) %>%
          dplyr::rename(variety = missing_var) %>%
          dplyr::bind_rows(data_options) %>%
          dplyr::arrange(id, rank) %>%
          dplyr::mutate(trait = rank) %>% # only have one trait.
          dplyr::select(-c("rank"))
      } else {
        data_options <- data_options %>%
          dplyr::filter(!is.na(variety)) %>%
          dplyr::group_by(id, trait) %>%
          dplyr::mutate(variety = if (dplyr::n_distinct(variety) == 1) NA else variety) %>%
          dplyr::filter(!is.na(variety)) %>%
          dplyr::ungroup() %>%
          unique()
        
        rank_2 <- data_options %>%
          dplyr::group_by(id, trait) %>%
          dplyr::filter(variety != na_value) %>%
          dplyr::count() %>%
          dplyr::mutate(rank = 2) %>%
          dplyr::filter(n == 2)
        
        data_options_b <- data_options %>%
          dplyr::full_join(rank_2) %>%
          dplyr::group_by(id, trait) %>%
          dplyr::mutate(variety = ifelse(is.na(variety), setdiff(possible_ranks, variety)[1], variety)) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(id, trait) %>%
          dplyr::full_join(data_options_id) %>%
          dplyr::select(c(id, trait, rank, dummy_variety = variety, variety = variable)) %>%
          tidyr::pivot_wider(names_from = trait, values_from = rank) %>%
          dplyr::filter(!is.na(variety))
      }
    } else {
      if (!is.null(data_plot_trait)){
        data_options_id <- data_options_id %>%
          dplyr::select(c(id, dummy_variety = variety, variable))
        data_options_a <- dplyr::full_join(data_options_a, data_options_id, by = c("id" = "id", "variety" = "variable"))
      } else {
        stop(paste0("No columns ending with ", trait_good, " or ", trait_bad, " found in `data`."))
      }
    }
  }
  
  # Combine if both datasets are available
  if ((!is.null(data) & !is.null(data_plot_trait)) && ncol(matching_cols) > 0) {
    message("Using both `data` and `data_plot_trait`")
    
    data_options <- dplyr::full_join(data_options_a, data_options_b)
    
    if (nrow(data_options_a) == nrow(data_options_b)) {
      if (nrow(data_options) != nrow(data_options_a)) {
        IDs_to_check <- data_options %>%
          dplyr::group_by(id) %>%
          dplyr::count() %>%
          dplyr::filter(n > 3) %>%
          dplyr::pull(id)
        warning("Using both data frames has caused repeats in trait columns.\nSome traits in `data` differ from `data_plot_trait` for the same ID and variety combination.\nCheck IDs: ",
                paste0(IDs_to_check, collapse = ", "))
      }
    }
  } else if (!is.null(data_plot_trait)) {
    data_options <- data_options_a
  } else {
    data_options <- data_options_b
  }
  
  return(data_options)
}

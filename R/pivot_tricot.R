#' Pivot Tricot Data
#'
#' This function transforms Tricot (Triadic Comparison of Technologies) data 
#' into a tidy format, assigning ranks based on trait evaluations.
#'
#' @param data A data frame containing Tricot rankings and trait evaluations.
#' @param id_var Character string specifying the ID column name. Default is `"id"`.
#' @param option_cols A character vector of column names representing the options evaluated. 
#'   Default is `c("option_a", "option_b", "option_c")`.
#' @param possible_ranks A character vector specifying the possible rank values.
#'   Default is `c("A", "B", "C")`.
#' @param trait_good Character string indicating the suffix for positive traits. Default is `"_pos"`.
#' @param trait_bad Character string indicating the suffix for negative traits. Default is `"_neg"`.
#' @param na_value The value used to indicate missing observations. Default is `"Not observed"`.
#'
#' @return A tibble with the transformed Tricot data, containing columns:
#'   - `id`: Identifier for each observation.
#'   - `trait`: Trait being evaluated.
#'   - `rank`: The rank assigned to each variety.
#'   - `variety`: The variety name.
#'
#' @export
#'
#' @examples
#' # Read in tricot data from the `gosset` library
#' cassava_pivot <- pivot_tricot(gosset::cassava)
pivot_tricot <- function(data,
                         id_var = "id",
                         option_cols = c("option_a", "option_b", "option_c"),
                         possible_ranks = c("A", "B", "C"),
                         trait_good = "_pos",
                         trait_bad = "_neg",
                         na_value = "Not observed"){
  
  label_map <- setNames(option_cols, possible_ranks)
  
  data_options <- data %>%
    dplyr::mutate(id = data[[id_var]]) %>%
    tidyr::pivot_longer(cols = all_of(option_cols),
                        names_to = "Label", values_to = "variable") %>%
    dplyr::mutate(Label = forcats::fct_recode(Label, !!!label_map))
  
  data_options_id <- data_options %>%
    dplyr::select(c(id, variety = Label, variable)) %>%
    unique()
  
  data_options <- data_options %>%
    tidyr::pivot_longer(cols = dplyr::all_of(c(dplyr::ends_with(trait_good),
                                               dplyr::ends_with(trait_bad))),
                        names_to = "trait",
                        values_to = "variety") %>%
    dplyr::select(c(id, trait, variety)) %>%
    dplyr::mutate(rank = ifelse(grepl(trait_good, trait), 1, 3)) %>%
    dplyr::mutate(trait = sub("(_[^_]*)$", "", trait)) %>%
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
  
  data_options <- data_options %>%
    dplyr::full_join(rank_2) %>%
    dplyr::group_by(id, trait) %>%
    dplyr::mutate(variety = ifelse(is.na(variety), setdiff(possible_ranks, variety)[1], variety)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id, trait) %>%
    dplyr::full_join(data_options_id) %>%
    dplyr::select(c(id, trait, rank, variety = variable)) %>%
    tidyr::pivot_wider(names_from = trait, values_from = rank) %>%
    dplyr::filter(!is.na(variety))
  
  return(data_options)
}
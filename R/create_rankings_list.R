#' Create a List of Rankings for Multiple Traits
#'
#' Converts a wide-format dataset with ranking values into a list of ranking objects,
#' one per trait, after filtering out any rows with missing values.
#'
#' @param data A data frame containing ranking information in wide format.
#' @param traits A character vector of column names corresponding to traits to be ranked.
#' @param id A string specifying the column name for individual identifiers.
#' @param variety A string specifying the column name for item (e.g., variety) labels.
#' @param group Logical. Whether to group rankings (e.g., for grouped ranking designs). Default is `FALSE`.
#'
#' @return A named list of ranking objects (one for each trait) generated using `gosset::rank_numeric`.
#' @export
#' 
#' @examples
#' library(gosset)
#' data(cassava)
#' cassava_by_id_variety <- pivot_tricot(cassava)
#' traits <- c("colour", "firmness", "odour", "overall", "stretchability", "taste", "mouldability", "smoothness")
#' rankings_list <- create_rankings_list(data = cassava_by_id_variety, traits, group = FALSE)
#' grouped_rankings_list <- create_rankings_list(data = cassava_by_id_variety, traits, group = TRUE)
create_rankings_list <- function(data, traits = traits, id = "id", variety = "variety", group = FALSE){
  data <- data %>%
    tidyr::pivot_longer(cols = all_of(traits), names_to = "trait", values_to = "rank") %>%
    dplyr::group_by(.data[[id]], trait) %>%
    dplyr::mutate(x = ifelse(any(is.na(rank)), 1, 0)) %>%
    dplyr::filter(x == 0)
  rankings_list <- traits %>%
    purrr::map(~ {
      data %>%
        dplyr::filter(trait == .x) %>%
        gosset::rank_numeric(data = ., 
                             items = variety, 
                             input = "rank", 
                             id = id, 
                             group = group,
                             ascending = TRUE)
    })
  names(rankings_list) <- traits
  cat("----------------\n")
  cat("Created Rankings Object")
  cat("----------------\n")
  return(rankings_list)
}

#' Summarise Data Levels for Multiple Datasets
#'
#' Applies `find_data_level()` to a named list of datasets and returns a summary table of their structural levels.
#'
#' @param data_list A named list of data frames to evaluate.
#' @param id_cols Character vector of possible names for ID columns.
#' @param variety_cols Character vector of possible names for variety columns.
#' @param trait_cols Character vector of possible names for trait columns.
#'
#' @return A tibble summarising the data level structure of each dataset. Columns include:
#' \describe{
#'   \item{dataset}{The name of the dataset.}
#'   \item{level}{The data level (e.g. `"id"`, `"id-variety"`).}
#'   \item{id_col}{The column used as the ID (if detected).}
#'   \item{variety_col}{The column used for varieties (if detected).}
#'   \item{trait_col}{The column used for traits (if detected).}
#' }
#'
#' @examples
#' data1 <- data.frame(id = 1:3)
#' data2 <- data.frame(id = 1:3, variety = letters[1:3])
#' summarise_data_levels(list(data1 = data1, data2 = data2))
#'
#' @export
summarise_data_levels <- function(data_list, id_cols = c("id", "participant_id", "participant_name", "ID"),
                                  variety_cols = c("variety", "varieties", "item", "items", "Genotype", "genotype"),
                                  trait_cols = c("trait", "traits")) {
  purrr::map_dfr(names(data_list), function(name) {
    res <- find_data_level(data_list[[name]], id_cols = id_cols, variety_cols = variety_cols, trait_cols = trait_cols)
    dplyr::tibble(
      dataset = name,
      level = res$level,
      id_col = res$id_col %||% NA,
      variety_col = res$variety_col %||% NA,
      trait_col = res$trait_col %||% NA,
    )
  })
}

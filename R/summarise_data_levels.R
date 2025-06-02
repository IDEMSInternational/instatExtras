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
#'   \item{level}{The data level (e.g. `"id"`, `"plot"`).}
#'   \item{id_col}{The column used as the ID (if detected).}
#'   \item{variety_col}{The column used for varieties (if detected).}
#'   \item{trait_col}{The column used for traits (if detected).}
#' }
#'
#' @examples
#' data1 <- data.frame(id = 1:3)
#' data2 <- data.frame(id = 1:3, variety = letters[1:3])
#' summarise_data_levels(list(data1 = data1, data2 = data2))
#' @export
summarise_data_levels <- function(data_list, id_cols = c("id", "participant_id", "participant_name", "ID"),
                                  variety_cols = c("variety", "varieties", "item", "items", "Genotype", "genotype"),
                                  trait_cols = c("trait", "traits")) {
  # If data_list is a character vector, get the datasets from the environment
  if (is.character(data_list)){
    if (exists("data_book", inherits = TRUE)) {
      data_list <- setNames(lapply(data_list, function(x) data_book$get_data_frame(x)), data_list)
      if (length(data_list) == 0 || all(sapply(data_list, nrow) == 0)) stop("Try giving data_list as variable not string.")
    } else {
      stop("Give data_list as variable not string.")
    }
  }
  
  output_data_levels <- purrr::map_dfr(names(data_list), function(name) {
    res <- find_data_level(data_list[[name]], id_cols = id_cols, variety_cols = variety_cols, trait_cols = trait_cols)
    dplyr::tibble(
      dataset = name,
      level = res$level,
      id_col = if (length(res$id_col) == 0) NA else res$id_col,
      variety_col = if (length(res$variety_col) == 0) NA else res$variety_col,
      trait_col = if (length(res$trait_col) == 0) NA else res$trait_col
    )
  })
  
  if (all(output_data_levels$level == "No marker columns found.")){
    output_data_levels$print <- "Tricot Data not found."
  } else {
    output_data_levels$print <- paste0(output_data_levels$dataset, " level: ", output_data_levels$level, collapse = "; ")
  }
  return(output_data_levels)
}

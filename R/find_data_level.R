#' Detect the Data Level of a Dataset
#'
#' Identifies the structure or "level" of a dataset based on common marker columns such as ID, variety, and trait. 
#' Returns the most granular level at which rows are uniquely identified, along with the detected column names.
#'
#' @param data A data frame to inspect.
#' @param id_cols Character vector of possible names for ID columns.
#' @param variety_cols Character vector of possible names for variety columns.
#' @param trait_cols Character vector of possible names for trait columns.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{level}{The detected level of the dataset, such as `"id"`, `"plot"`, `"plot-trait"`, etc.}
#'   \item{id_col}{The name of the column identified as the ID column (if found).}
#'   \item{variety_col}{The name of the variety column (if found).}
#'   \item{trait_col}{The name of the trait column (if found).}
#' }
#'
#' @examples
#' df <- data.frame(ID = 1:3, item = letters[1:3], trait = rep("height", 3))
#' find_data_level(df)
#' 
#' df <- data.frame(ID = c(1:3, 1:3), variety = c(1,1,1,3,3,3))
#' find_data_level(df)
#' @export
find_data_level <- function(data,
                            id_cols = c("id", "participant_id", "participant_name", "ID"),
                            variety_cols = c("variety", "varieties", "item", "items", "Genotype", "genotype"),
                            trait_cols = c("trait", "traits")) {
  
  selected_cols <- list(id = NULL, variety = NULL, trait = NULL)
  
  rename_to_standard <- function(df, possible_names, standard_name) {
    found <- intersect(possible_names, names(df))
    if (length(found) > 1) {
      found <- found[1]
      message(paste0("Multiple matching variables for '", standard_name, "'. Taking ", found, "."))
    }
    
    selected_cols[[standard_name]] <<- found
    df <- df %>% dplyr::rename(!!standard_name := dplyr::all_of(found))
    return(df)
  }
  
  data_renamed <- data %>% dplyr::select(any_of(c(id_cols, variety_cols, trait_cols))) %>%
    rename_to_standard(id_cols, "id") %>%
    rename_to_standard(variety_cols, "variety") %>%
    rename_to_standard(trait_cols, "trait")
  
  markers <- intersect(c("id", "variety", "trait"), names(data_renamed))
  
  if (length(markers) == 0) {
    return(list(
      level = "No marker columns found.",
      id_col = selected_cols$id,
      variety_col = selected_cols$variety,
      trait_col = selected_cols$trait
    ))
  }
  
  marker_combinations <- unlist(
    lapply(seq_along(markers), function(n) {
      combn(markers, n, simplify = FALSE)
    }),
    recursive = FALSE
  )
  
  results <- purrr::map(marker_combinations, function(cols) {
    grouped <- data_renamed %>% dplyr::distinct(dplyr::across(dplyr::all_of(cols)))
    dplyr::tibble(
      level = paste(cols, collapse = "-"),
      is_unique = nrow(grouped) == nrow(data_renamed)
    )
  })
  
  result_df <- dplyr::bind_rows(results)
  
  level_string <- if (any(result_df$is_unique)) {
    best <- result_df %>%
      dplyr::filter(is_unique) %>%
      dplyr::arrange(lengths(strsplit(level, "-"))) %>%
      dplyr::slice(1)
    level_raw <- best$level
    
    # Rename specific levels
    level_standard <- dplyr::case_when(
      level_raw == "id-variety" ~ "plot",
      level_raw == "id-variety-trait" ~ "plot-trait",
      TRUE ~ level_raw
    )
  } else {
    "No combination of markers uniquely identifies the data rows."
  }

  return(list(
    level = level_string,
    id_col = selected_cols$id,
    variety_col = selected_cols$variety,
    trait_col = selected_cols$trait
  ))
}
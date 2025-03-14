#' Convert Data to Character Matrix
#' 
#' This function converts the input data to a character matrix, with options for formatting decimal places and handling missing values.
#'
#' @param data The input data to be converted.
#' @param format_decimal_places A logical value indicating whether decimal places should be formatted. Default is `TRUE`.
#' @param decimal_places A numeric vector specifying the number of decimal places for each column. If not provided, it uses the function `get_default_significant_figures` to determine the number of decimal places.
#' @param is_scientific A logical vector indicating whether scientific notation should be used for each column. Default is `FALSE`.
#' @param return_data_frame A logical value indicating whether the result should be returned as a data frame. Default is `TRUE`.
#' @param na_display A character value specifying how missing values should be displayed. If not provided, missing values are left as is.
#' @param check.names A logical value indicating whether column names should be checked and modified if necessary. Default is `TRUE'.
#'
#' @return The converted data as a character matrix or data frame, depending on the value of `return_data_frame`.
#'
#' @export
#'
#' @examples
#' data <- data.frame(A = c(1.234, 5.678), B = c(10.123, 20.456))
#' converted <- convert_to_character_matrix(data)
#' print(converted)
#' 
#' # example with SFC data
#' points <- sf::st_sfc(sf::st_point(c(1, 2)), sf::st_point(c(3, 4)))
#' sf_data <- data.frame(id = c(1, 2))
#' sf_data$geometry <- points
#' class(sf_data$geometry) <- c("sfc", class(sf_data$geometry))  # Ensure "sfc" class
#' # Run function
#' result <- convert_to_character_matrix(sf_data)
#' 
#' @seealso
#' \code{\link{get_default_significant_figures}}
convert_to_character_matrix <- function(data, format_decimal_places = TRUE, decimal_places, is_scientific = FALSE, return_data_frame = TRUE, na_display = NULL, check.names = TRUE) {
  if(nrow(data) == 0) {
    out <- matrix(nrow = 0, ncol = ncol(data))
    colnames(out) <- colnames(data)
  } else {
    out = matrix(nrow = nrow(data), ncol = ncol(data))
    if(!format_decimal_places){
      decimal_places=rep(NA, ncol(data)) 
    } else if(missing(decimal_places)){
      decimal_places = sapply(data, instatExtras::get_default_significant_figures)
    } 
    i = 1
    for (curr_col in colnames(data)) {
      #if its a geometry list-column then convert to text using sf package.
      #see issue #7165
      if ("sfc" %in% class(data[[i]])) {
        out[, i] <- sf::st_as_text(data[[i]])
      } else if (is.na(decimal_places[i])) {
        #use as.character() for non numeric column vales because format() adds extra spaces to the text
        #which are recognised oddly by the R.Net
        out[, i] <- as.character(data[[i]])
      } else {
        out[,i] <- format(data[[i]], digits = decimal_places[i], scientific = is_scientific[i])
      }
      if (!is.null(na_display)) {
        out[is.na(data[[i]]), i] <- na_display
      }
      i = i + 1
    }
    colnames(out) <- colnames(data)
    rownames(out) <- rownames(data)
  }
  if(return_data_frame){
    out <- data.frame(out, stringsAsFactors = FALSE, check.names = check.names)
  } 
  return(out)
}
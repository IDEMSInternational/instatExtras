#' Extract SPEI/SPI Column from 'spei' Object
#'
#' @description
#' 
#' This function extracts the Standardized Precipitation-Evapotranspiration Index (SPEI) or Standardized Precipitation Index (SPI) column from a 'spei' object. It is designed to work with the original data and handle multiple stations if present. The function removes NA values introduced when unstacking the data to return a vector of the correct length.
#' 
# It requires the original data in order to return a vector of the correct length by removing NA values introduced when unstacking.
# An alternative to this is to have a single wrapper SPEI/SPI function to handle this.
# The advantage of this method is that it doesn't hide the call to SPEI/SPI in R-Instat and is compatible with the existing dialog.
# The wrapper function may be a prefered long-term solution.
#' 
#' @param x An object of class 'spei'.
#' @param data The data.frame to calculate from.
#' @param station The name of the station column in \code{data}, if the data are for multiple station.
#' @param year The name of the year column in \code{data}.
#' @param month The name of the month column in \code{data}.
#'
#' @return A vector containing the extracted SPEI/SPI column from the 'spei' object 'x'.
#' @export
#'
#' @examples # TODO:
#' 
#' 
#' 
spei_output <- function(x, data, station, year, month) {
  if (! inherits(x, "spei")) stop("x must be an object of class 'spei'")
  vals <- x$fitted
  # If is.mts then multiple stations. Need to unstack and merge to ensure correct values obtained.
  if (stats::is.mts(vals)) {
    df_spei <- as.data.frame(vals)
    # ind will be the year in fractions
    df_spei$ind <- zoo::index(x$fitted)
    # Stack all stations to get back into tidy format.
    df_spei <- tidyr::pivot_longer(df_spei, cols = 1:ncol(vals))
    # Integer part is year
    df_spei$yy <- trunc(df_spei$ind)
    # Remainder is fraction of month. Use round to ensure exact integers for merging.
    df_spei$mm <- ((df_spei$ind - df_spei$yy) * 12) + 1
    df_spei$mm <- round(df_spei$mm)
    if (!(is.numeric(data[[month]]) | is.factor(data[[month]]))) stop("month must be numeric or factor to ensure SPEI/SPI values are calculated correctly.")
    # If factor, this assumes levels are in correct order.
    data[[month]] <- as.numeric(data[[month]])
    by <- c("name", "yy", "mm")
    names(by) <- c(station, year, month)
    # Need to merge to know which NA values are true and which were introduced when unstacking.
    df_new <- dplyr::left_join(data, df_spei, by = by)
    col <- df_new$value
  } else {
    # If single station, then no extra missing values were introduced. Data just needs to be made into a vector.
    col <- as.vector(vals)
  }
  col
  
}
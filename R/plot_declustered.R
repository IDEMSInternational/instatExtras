#' Generate declustering plots
#' 
#' @description
#' This function generates declustering plots based on the provided data and parameters. It uses the texmex package for declustering analysis.
#' 
#' @param data A data frame containing the input data.
#' @param station_col_name The name of the column in the data frame that represents the stations.
#' @param element_col_name The name of the column in the data frame that represents the elements.
#' @param threshold The threshold value used for declustering.
#' @param r The run length parameter used for declustering. Default is NULL.
#' @param xlab The label for the x-axis of the plot. Default is NULL.
#' @param ylab The label for the y-axis of the plot. Default is NULL.
#' @param ncol The number of columns in the plot grid. Default is 1.
#' @param print_summary If TRUE, it prints the declustering summary for each station. Default is FALSE.
#'
#' @return If print_summary is FALSE, it returns a plot object(s). Otherwise, it returns NULL.
#' 
#' @export
#'
#' @examples
#' # Example usage
#' # data <- read.csv("data.csv")
#' # plot_declustered(data, "station", "element", threshold = 0.5)
#' 
#' # Example usage with additional parameters
#' # plot_declustered(data, "station", "element", threshold = 0.5, r = 10, xlab = "Time", ylab = "Value", ncol = 2, print_summary = TRUE)
#' 
plot_declustered <- function(data, station_col_name, element_col_name, threshold, r = NULL, xlab = NULL, ylab = NULL, ncol = 1, print_summary = FALSE) {
  if (!missing(station_col_name)) {
    plts <- list()
    station_col <- data[, station_col_name]
    stations <- unique(station_col)
    for (i in seq_along(stations)) {
      station <- stations[i]
      d <- data[station_col == station, ]
      obj <- texmex::declust(y = stats::na.exclude(d[, element_col_name]), r = r, threshold = threshold)
      if (print_summary) {
        cat("Station:", paste0("", station, ""), "\n \n")
        cat("Threshold", obj$threshold, "\n")
        cat("Declustering using the intervals method, run length", obj$r, "\n")
        cat("Identified", obj$nClusters, "clusters.", "\n")
        cat("------------------------------------------------------", "\n \n")
      } else {
        plts[[i]] <- obj %>%
          ggplot2::ggplot(xlab = xlab, ylab = ylab, main = stations[i])
      }
    }
    if (!print_summary) {
      patchwork::wrap_plots(plts, ncol = ncol)
    }
  }
  else {
    obj <- texmex::declust(y = stats::na.exclude(data[, element_col_name]), r = r, threshold = threshold)
    if (print_summary) {
      cat("Threshold", obj$threshold, "\n")
      cat("Declustering using the intervals method, run length", obj$r, "\n")
      cat("Identified", obj$nClusters, "clusters.", "\n")
    } else {
      obj %>% ggplot2::ggplot(xlab = xlab, ylab = ylab)
    }
  }
}
#' Plot Mean Residual Life
#' 
#' @description
#' This function generates a plot of the mean excess for a given threshold in a dataset.
#' 
#' @param data A data frame containing the dataset.
#' @param station_name The name of the column in `data` representing the stations.
#' @param element_name The name of the column in `data` representing the elements.
#' @param umin The lower threshold value. If not provided, the minimum value in `element_name` will be used.
#' @param umax The upper threshold value. If not provided, the maximum value in `element_name` will be used.
#' @param ncol The number of columns for arranging the subplots. Default is 1.
#' @param xlab The label for the x-axis. Default is "Threshold".
#' @param ylab The label for the y-axis. Default is "Mean excess".
#' @param fill The fill color for the plot. Default is "red".
#' @param col The color for the plot. Default is "black".
#' @param rug A logical value indicating whether to include rug plot. Default is TRUE.
#' @param addNexcesses A logical value indicating whether to add the number of excesses. Default is TRUE.
#' @param textsize The size of the text in the plot. Default is 4.
#'
#' @return If `station_name` is provided, a grid of plots is returned. Otherwise, a single plot is returned.
#'
#' @export
#'
#' @examples
#' # Example 1: Single plot
#' # Generate a data frame
#' # data <- data.frame(
#' #  station = c("A", "A", "B", "B", "C", "C"),
#' #  element = c(10, 15, 20, 25, 30, 35)
#' # )
#'
#' # Generate a plot for the element column with default settings
#' # plot_mrl(data, element_name = "element")
#'
#' # Example 2: Grid of plots
#' # Generate a data frame
#' # data <- data.frame(
#' #  station = c("A", "A", "B", "B", "C", "C"),
#' #  element = c(10, 15, 20, 25, 30, 35)
#' # )
#'
#' # Generate a grid of plots for each station
#' # plot_mrl(data, station_name = "station", element_name = "element", ncol = 2)
#' 
plot_mrl <- function(data, station_name, element_name, umin, umax, ncol = 1,
                     xlab = "Threshold", ylab = "Mean excess", fill = "red",
                     col = "black", rug = TRUE, addNexcesses = TRUE, textsize = 4) {
  if (!missing(station_name)) {
    plts <- list()
    station_col <- data[, station_name]
    stations <- unique(station_col)
    for (i in seq_along(stations)) {
      d <- data[station_col == stations[i], ]
      element_col <- d[, element_name]
      if (missing(umin)) {
        umin <- min(element_col, na.rm = TRUE)
      }
      if (missing(umax)) {
        umax <- max(element_col, na.rm = TRUE)
      }
      plts[[i]] <- texmex::mrl(stats::na.exclude(element_col), umin = umin, umax = umax) %>%
        ggplot2::ggplot(xlab = xlab, ylab = ylab, main = stations[i], fill = fill,
                        col = col, rug = rug, addNexcesses = addNexcesses, textsize = textsize
        )
    }
    patchwork::wrap_plots(plts, ncol = ncol)
  }
  else {
    element_col <- data[, element_name]
    if (missing(umin)) {
      umin <- min(element_col, na.rm = TRUE)
    }
    if (missing(umax)) {
      umax <- max(element_col, na.rm = TRUE)
    }
    texmex::mrl(data = stats::na.exclude(element_col), umin = umin, umax = umax) %>%
      ggplot2::ggplot(xlab = xlab, ylab = ylab, fill = fill, col = col, rug = rug, 
                      addNexcesses = addNexcesses, textsize = textsize
      )
  }
}

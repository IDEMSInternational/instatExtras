#' OTHER ROSE PLOTS
#'
#'@description
#'This function creates a wrapper around functions from openair package for generating various types of rose plots.
#'
#' @param data A data frame containing the required variables.
#' @param type1_col_name The column name for the first type variable.
#' @param type2_col_name The column name for the second type variable.
#' @param date_col_name The column name for the date variable.
#' @param wd_col_name The column name for the wind direction variable.
#' @param ws_col_name The column name for the wind speed variable.
#' @param main_method The main method to be used for generating the rose plots. Valid options are "percentile_rose", "polar_plot", "polar_annulus", "polar_cluster", and "polar_frequency".
#' @param single_pollutant The name of the pollutant variable to be used for single-pollutant plots.
#' @param multiple_pollutant The name(s) of the pollutant variable(s) to be used for multiple-pollutant plots.
#' @param ... Additional arguments to be passed to the openair package functions.
#'
#' @return The function generates the desired rose plot based on the specified parameters.
#'
#' @export
#'
#' @examples
#' data <- read.csv("data.csv")
#' other_rose_plots(data, "Type1", "Type2", "Date", "WD", "WS", "percentile_rose", "PM2.5", c("NO2", "SO2"))
#'
#' This example generates a percentile rose plot using the "percentile_rose" method from the openair package, using the "PM2.5" pollutant for the plot and including "NO2" and "SO2" as additional pollutants in the plot.
#'
#' @importFrom openair percentileRose polarPlot polarAnnulus polarCluster polarFreq
#' @import dplyr
#' @importFrom utils rename
other_rose_plots <- function(data, type1_col_name, type2_col_name, date_col_name, wd_col_name, ws_col_name, main_method, single_pollutant, multiple_pollutant, ...) {
  type <- "default"
  if (!missing(type1_col_name) && !missing(type2_col_name)) {
    type <- c(type1_col_name, type2_col_name)
  }
  if (missing(type1_col_name) && !missing(type2_col_name)) {
    type <- type2_col_name
  }
  if (!missing(type1_col_name) && missing(type2_col_name)) {
    type <- type1_col_name
  }
  if (!main_method %in% c("percentile_rose", "polar_plot", "polar_annulus", "polar_cluster", "polar_frequency")) stop("Method must be either percentile_rose, polar_plot, polar_annulus, polar_cluster, or polar_frequency.")
  if (missing(data)) stop("Data is missing with no default.")
  col_names <- colnames(data)
  fun_col_names <- c(date_col_name, wd_col_name)
  if (!all(fun_col_names %in% col_names)) stop(paste(fun_col_names[!fun_col_names %in% col_names], "column(s) missing in the dataframe."))
  if (!"date" %in% col_names) data <- dplyr::rename(data, date = !!date_col_name)
  if (!"wd" %in% col_names) data <- dplyr::rename(data, wd = !!wd_col_name)
  if (main_method == "percentile_rose") {
    if
    
    (!"ws" %in% col_names) data <- dplyr::rename(data, ws = !!ws_col_name)
    openair::percentileRose(mydata = data, type = type, pollutant = multiple_pollutant, ...)
  } else if (main_method == "polar_plot") {
    openair::polarPlot(mydata = data, type = type, pollutant = multiple_pollutant,  ...)
  } else if (main_method == "polar_annulus") {
    if (!"ws" %in% col_names) data <- dplyr::rename(data, ws = !!ws_col_name)
    openair::polarAnnulus(mydata = data, type = type, pollutant = multiple_pollutant,  ...)
  } else if (main_method == "polar_cluster") {
    openair::polarCluster(mydata = data, type = type, pollutant = single_pollutant, ...)
  } else if (main_method == "polar_frequency") {
    if (!"ws" %in% col_names) data <- dplyr::rename(data, ws = !!ws_col_name)
    openair::polarFreq(mydata = data, type = type, pollutant = single_pollutant, ...)
  }
}
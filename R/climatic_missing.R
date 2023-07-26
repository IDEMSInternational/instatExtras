#' Summarise missing climatic values
#' 
#' @description
#' This function calculates the summary of missing data in climatic variables for different stations and elements within a specified time period. It provides information on the start and end dates of missing data, the total number of missing values, and the percentage of missing values for each station and element. Additionally, it counts the number of complete years (without any missing values) for each station and element.
#' 
#' @param data A data frame containing climatic data.
#' @param date A variable representing the date or time in the data frame.
#' @param elements A character vector specifying the climatic elements to be analyzed. Defaults to all elements in the data frame.
#' @param stations A character vector specifying the stations to be analyzed. If not provided, the analysis will be performed for all stations in the data frame.
#' @param start A logical value indicating whether to determine the start date of missing data for each station and element. If set to TRUE, the function finds the first date with missing values. If set to FALSE, the function uses the first date in the specified time period.
#' @param end A logical value indicating whether to determine the end date of missing data for each station and element. If set to TRUE, the function finds the last date with missing values. If set to FALSE, the function uses the last date in the specified time period.
#' 
#' @return A data frame containing the summary of missing data, including the start and end dates, the total number of missing values, the percentage of missing values, and the number of complete years for each station and element.
#' @export
#'
#' @examples
#' # Example 1: Calculate missing data summary for all elements and stations
#' #summary_data <- climatic_missing(data = climatic_data, date = "Date")
#' 
#' # Example 2: Calculate missing data summary for specific elements and stations
#' #summary_data <- climatic_missing(data = climatic_data, date = "Date", elements = c("Temperature", "Precipitation"), stations = c("Station1", "Station2"))
#' 
#' # Example 3: Calculate missing data summary with custom start and end dates
#' #summary_data <- climatic_missing(data = climatic_data, date = "Date", start = FALSE, end = TRUE)
#' 
#' # Example 4: Calculate missing data summary without including station information
#' #summary_data <- climatic_missing(data = climatic_data, date = "Date", stations = NULL)
#' 
#' 
climatic_missing <- function(data, date, elements = ..., stations,
                             start = TRUE, end = FALSE){
  if (missing(date)){
    stop('argument "date" is missing, with no default')
  }
  if (missing(elements)){
    stop('argument "elements" is missing, with no default')
  }
  # stack data
  data.stack <- data %>%
    tidyr::pivot_longer(cols = c({{ elements }}),
                        names_to = "Element",
                        values_to = "value")
  # sort start/end times
  # set start date
  if (start){
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(start = ({{ date }})[which.min(is.na( value ))])
    
  }else{
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}) %>%
      dplyr::mutate(start = dplyr::first( {{ date }} ))
  }
  # set end date
  if (end){
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }}, Element ) %>%
      dplyr::mutate(end = ({{ date }} )[dplyr::last(which(!is.na( value )))])
  }else{
    data.stack <- data.stack %>%
      dplyr::group_by({{ stations }} ) %>%
      dplyr::mutate(end = dplyr::last({{ date }}))
  }
  # number and percentage missing
  summary.data <- data.stack %>%
    dplyr::group_by({{ stations }}, Element) %>%
    dplyr::filter(({{ date }}) >= start & ({{ date }}) <= end) %>%
    dplyr::summarise(From = dplyr::first(start),
                     To = dplyr::last(end),
                     Missing = sum(is.na(value)),
                     `%` = round(sum(is.na(value))/dplyr::n()*100, 1))
  
  # complete years
  complete.years <- data.stack %>%
    dplyr::group_by({{ stations }}) %>%
    dplyr::filter(({{ date }}) >= start & ({{ date }}) <= end) %>%
    dplyr::group_by(lubridate::year({{ date }}), {{ stations }}, Element) %>%
    dplyr::summarise(count = sum(is.na(value)))
  complete.years <- complete.years %>%
    dplyr::group_by({{ stations }}, Element) %>%
    dplyr::summarise(Full_Years = sum(count == 0))
  
  
  # bind together
  summary.data <- merge(summary.data, complete.years)
  
  if (missing(stations)){
    summary.data$stations <- NULL
  }
  
  return(summary.data)
}
#' Climatic Details
#' 
#' @description
#' This function extracts climatic details from a given dataset based on specified parameters such as date, elements, stations, and level. It provides information about the duration and frequency of missing values for each element at different levels (day, month, year).
#' 
#' @param data A dataset containing climatic data.
#' @param date The date variable in the dataset.
#' @param elements A character vector specifying the climatic elements to analyze.
#' @param stations A character vector specifying the stations to analyze.
#' @param order A logical value indicating whether the resulting tables should be ordered.
#' @param day A logical value indicating whether to include information at the day level.
#' @param month A logical value indicating whether to include information at the month level.
#' @param year A logical value indicating whether to include information at the year level.
#' @param level A logical value indicating whether to include the level information in the output.
#'
#' @return A data frame containing climatic details such as start and end dates of missing values and the count of missing values for each element at the specified levels.
#'
#' @export
#'
#' @examples
#' #climatic_details(data = climatic_data, 
#' #                 date = "Date", 
#' #                 elements = c("Temperature", "Precipitation"), 
#' #                 stations = c("Station1", "Station2"), 
#' #                 order = TRUE, 
#' #                 day = TRUE, 
#' #                 month = FALSE, 
#' #                 year = TRUE, 
#' #                 level = TRUE)

climatic_details <- function(data, date, elements = ..., stations,
                             order = FALSE,
                             day = TRUE,
                             month = FALSE,
                             year = FALSE, level = FALSE){
  
  
  if (missing(date)){
    stop('argument "date" is missing, with no default')
  }
  
  if (missing(elements)){
    stop('argument "elements" is missing, with no default')
  }
  
  i <- 0
  list_tables <- NULL
  
  # stack data
  data.stack <- data %>%
    tidyr::pivot_longer(cols = c({{ elements }}),
                        names_to = "Element",
                        values_to = "Value") %>%
    dplyr::mutate(Element = make_factor(Element))
  
  # sort start/end times
  
  if (!any(day, month, year)){
    warning('At least one of day, month, year need to be selected')
  }
  
  if (day){
    i = i + 1
    detail.table.day = data.stack %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(element.na = data.table::rleid(Value)) %>%
      dplyr::filter(is.na(Value)) %>%
      dplyr::group_by(element.na, {{ stations }}, Element) %>%
      dplyr::summarise(From = dplyr::first({{ date }}),
                       To = dplyr::last({{ date }}),
                       Count = dplyr::n()) %>%
      dplyr::mutate(Level = "Day")
    
    if (order){
      detail.table.day <- detail.table.day %>% dplyr::arrange(From)
    } else {  
      detail.table.day <- detail.table.day %>% dplyr::arrange(Element)
    }
    
    detail.table.day <- detail.table.day %>% dplyr::ungroup() %>% dplyr::select(-c("element.na"))
    list_tables[[i]] <- detail.table.day
    
  }
  
  if (month){
    i = i + 1
    detail.table.month <- data.stack %>%
      dplyr::mutate(Date.ym = zoo::as.yearmon({{ date }}))  %>%
      dplyr::group_by(Date.ym, {{ stations }}, Element)
    
    detail.table.month <- detail.table.month %>%
      dplyr::summarise(no = dplyr::n(),
                       na = sum(is.na(Value)),
                       From = dplyr::first({{ date }}),
                       To = dplyr::last({{ date }})) %>%
      dplyr::mutate(is.complete = ifelse(no == na, 1, 0)) # 0 if all are missing
    
    detail.table.month <- detail.table.month %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(element.na = data.table::rleid(is.complete)) %>%
      dplyr::filter(is.complete == 1) %>%
      dplyr::group_by(element.na, {{ stations }}, Element) %>%
      dplyr::summarise(From = dplyr::first(From),
                       To = dplyr::last(To),
                       Count = dplyr::n()) %>%
      dplyr::mutate(Level = "Month")
    
    if (order){
      detail.table.month <- detail.table.month %>% dplyr::arrange(From)
    } else {
      detail.table.month <- detail.table.month %>% dplyr::arrange(Element)
    }
    
    detail.table.month <- detail.table.month %>% dplyr::ungroup() %>% dplyr::select(-c("element.na"))
    list_tables[[i]] <- detail.table.month
  }
  
  if (year) {
    i = i + 1
    detail.table.year <- data.stack %>%
      dplyr::mutate(Date.y = lubridate::year({{ date }}))  %>%
      dplyr::group_by(Date.y, {{ stations }}, Element)
    
    detail.table.year <- detail.table.year %>%
      dplyr::summarise(no = dplyr::n(),
                       na = sum(is.na(Value)),
                       From = dplyr::first({{ date }}),
                       To = dplyr::last({{ date }})) %>%
      dplyr::mutate(is.complete = ifelse(no == na, 1, 0)) # 0 if all are missing
    
    detail.table.year <- detail.table.year %>%
      dplyr::group_by({{ stations }}, Element) %>%
      dplyr::mutate(element.na = data.table::rleid(is.complete)) %>%
      dplyr::filter(is.complete == 1) %>%
      dplyr::group_by(element.na, {{ stations }}, Element) %>%
      dplyr::summarise(From = dplyr::first(From),
                       To = dplyr::last(To),
                       Count = dplyr::n()) %>%
      dplyr::mutate(Level = "Year")
    
    if (order){
      detail.table.year <- detail.table.year %>% dplyr::arrange(From)
    } else {
      detail.table.year <- detail.table.year %>% dplyr::arrange(Element)
    }
    
    detail.table.year <- detail.table.year %>% dplyr::ungroup() %>% dplyr::select(-c("element.na"))
    list_tables[[i]] <- detail.table.year
  }
  
  detail.table.all <- plyr::ldply(list_tables, data.frame) %>%
    dplyr::mutate(Level = make_factor(Level))
  
  return(detail.table.all)
  
}
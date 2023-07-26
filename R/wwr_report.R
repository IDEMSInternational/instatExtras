#'Reshape data into formats required by WMO for submission of climatic data
#'
#' @description
#'The function is meant to reshape data into formats required by WMO for submission of climatic data.This gives Yearly data records with monthly and annual data for a particular year:
#' 
#' 
#' @param data The input data frame containing the climatic data.
#' @param year The name of the column in 'data' representing the year.
#' @param month The name of the column in 'data' representing the month.
#' @param mean_station_pressure The name of the column in 'data' representing the mean station pressure.
#' @param mean_sea_level_pressure The name of the column in 'data' representing the mean sea level pressure.
#' @param mean_temp The name of the column in 'data' representing the mean daily air temperature.
#' @param total_precip The name of the column in 'data' representing the total precipitation.
#' @param mean_max_temp The name of the column in 'data' representing the mean daily maximum air temperature.
#' @param mean_min_temp The name of the column in 'data' representing the mean daily minimum air temperature.
#' @param mean_rel_hum The name of the column in 'data' representing the mean of the daily relative humidity.
#' @param link The name of the column in 'data' representing the link between the data and station information.
#' @param link_by The method for linking the data and station information. Possible values: "wmo_number", "station_name".
#' @param station_data The data frame containing the station information.
#' @param wmo_number The name of the column in 'station_data' representing the WMO number.
#' @param latitude The name of the column in 'station_data' representing the latitude.
#' @param longitude The name of the column in 'station_data' representing the longitude.
#' @param country_name The name of the column in 'station_data' representing the country name.
#' @param station_name The name of the column in 'station_data' representing the station name.
#' @param height_station The name of the column in 'station_data' representing the station height.
#' @param height_barometer The name of the column in 'station_data' representing the barometer height.
#' @param wigos_identifier The name of the column in 'station_data' representing the WIGOS station identifier.
#' @param folder The folder where the output files will be saved.
#'
#' @return TODO
#'
#' @examples # TODO
#' 
wwr_export <- function(data, year, month, mean_station_pressure, mean_sea_level_pressure, 
                       mean_temp, total_precip, mean_max_temp, mean_min_temp, mean_rel_hum, link, link_by,
                       station_data, wmo_number, latitude, longitude, country_name, station_name, 
                       height_station, height_barometer, wigos_identifier, folder) {
  
  stopifnot(link_by %in% c("wmo_number", "station_name"))
  if (any(nchar(station_data[[year]]) != 4)) stop("year must be a 4 digit number.")
  if (!missing(wmo_number)) {
    # Convert to character to avoid incorrect 
    if (is.factor(station_data[[wmo_number]])) station_data[[wmo_number]] <- as.character(station_data[[wmo_number]])
    if (any(is.na(as.numeric(station_data[[wmo_number]])))) stop("wmo_number must not contain missing values and must be a number.")
    if (any(nchar(as.character(station_data[[wmo_number]])) > 5, na.rm = TRUE)) stop("wmo_number must be no more than 5 digits.")
  }
  
  if (link_by == "wmo_number") {
    station_link <- wmo_number
  } else station_link <- station_name
  if (!all(unique(data[[link]]) %in% station_data[[station_link]])) {
    stop("station_data is missing information for the following stations
         found in the data:",
         paste(which(!unique(data[[link]]) %in% station_data[[wmo_number]]), collapse = ", "))
  }
  if (!missing(wmo_number)) {
    station_data[[wmo_number]] <- as.numeric(station_data[[wmo_number]])
    station_data[[wmo_number]] <- ifelse(is.na(station_data[[wmo_number]]),
                                         "", sprintf("%05d", station_data[[wmo_number]]))
  } else {
    wmo_number <- ".wmo_number"
    station_data[[wmo_number]] <- ""
  }
  station_data[[latitude]] <- dd_to_dms(station_data[[latitude]], lat = TRUE)
  station_data[[longitude]] <- dd_to_dms(station_data[[longitude]], lat = FALSE)
  if (!missing(height_station)) {
    station_data[[height_station]] <- ifelse(is.na(station_data[[height_station]]),
                                             "", round(station_data[[height_station]]))
  } else {
    height_station <- ".height_station"
    station_data[[height_station]] <- ""
  }
  if (!missing(height_barometer)) {
    station_data[[height_barometer]] <- ifelse(is.na(station_data[[height_barometer]]),
                                               "", round(station_data[[height_barometer]], 1))
  } else {
    height_barometer <- ".height_barometer"
    station_data[[height_barometer]] <- ""
  }
  if (missing(wigos_identifier)) {
    wigos_identifier <- ".wigos_identifier"
    station_data[[wigos_identifier]] <- ""
  }
  if (!missing(mean_station_pressure)) {
    df_2_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", round(summary_mean(.data[[mean_station_pressure]], na.rm = TRUE), 1)), .groups = "keep")
    data[[mean_station_pressure]] <- ifelse(is.na(data[[mean_station_pressure]]), 
                                            "", round(data[[mean_station_pressure]], 1))
    data[[mean_station_pressure]] <- sprintf("%6s", data[[mean_station_pressure]])
    df_2 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(mean_station_pressure),
                                        values_fill = strrep(" ", 6))
  }
  if (!missing(mean_sea_level_pressure)) {
    df_3_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", round(summary_mean(.data[[mean_sea_level_pressure]], na.rm = TRUE), 1)), .groups = "keep")
    data[[mean_sea_level_pressure]] <- ifelse(is.na(data[[mean_sea_level_pressure]]), 
                                              "", round(data[[mean_sea_level_pressure]], 1))
    data[[mean_sea_level_pressure]] <- sprintf("%6s", data[[mean_sea_level_pressure]])
    df_3 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(mean_sea_level_pressure),
                                        values_fill = strrep(" ", 6))
  }
  if (!missing(mean_temp)) {
    df_4_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", round(summary_mean(.data[[mean_temp]], na.rm = TRUE), 1)), .groups = "keep")
    data[[mean_temp]] <- ifelse(is.na(data[[mean_temp]]), 
                                "", round(data[[mean_temp]], 1))
    data[[mean_temp]] <- sprintf("%6s", data[[mean_temp]])
    df_4 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(mean_temp),
                                        values_fill = strrep(" ", 6))
  }
  if (!missing(total_precip)) {
    df_5_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", format(summary_sum(.data[[total_precip]], na.rm = TRUE), digits = 1, nsmall = 1)), .groups = "keep")
    data[[total_precip]] <- ifelse(is.na(data[[total_precip]]), 
                                   "", ifelse(data[[total_precip]] <= 0.05, 0, format(data[[total_precip]], digits = 1, nsmall = 1)))
    data[[total_precip]] <- sprintf("%6s", data[[total_precip]])
    df_5 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(total_precip),
                                        values_fill = strrep(" ", 6))
  }
  if (!missing(mean_max_temp)) {
    df_6_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", round(summary_mean(.data[[mean_max_temp]], na.rm = TRUE), 1)), .groups = "keep")
    data[[mean_max_temp]] <- ifelse(is.na(data[[mean_max_temp]]), 
                                    "", round(data[[mean_max_temp]], 1))
    data[[mean_max_temp]] <- sprintf("%6s", data[[mean_max_temp]])
    df_6 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(mean_max_temp),
                                        values_fill = strrep(" ", 6))
  }
  if (!missing(mean_min_temp)) {
    df_7_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", round(summary_mean(.data[[mean_min_temp]], na.rm = TRUE), 1)), .groups = "keep")
    data[[mean_min_temp]] <- ifelse(is.na(data[[mean_min_temp]]), 
                                    "", round(data[[mean_min_temp]], 1))
    data[[mean_min_temp]] <- sprintf("%6s", data[[mean_min_temp]])
    df_7 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(mean_min_temp),
                                        values_fill = strrep(" ", 6))
  }
  if (!missing(mean_rel_hum)) {
    if (any(data[[mean_rel_hum]] < 0 | data[[mean_rel_hum]] > 100, na.rm = TRUE)) stop("Mean Relative Humidity must be a percentage between 0 and 100.")
    df_8_means <- data %>%
      dplyr::group_by(!!! rlang::syms(c(link, year))) %>%
      dplyr::summarise(mean = sprintf("%6s", round(summary_mean(.data[[mean_rel_hum]], na.rm = TRUE), 0)), .groups = "keep")
    data[[mean_rel_hum]] <- ifelse(is.na(data[[mean_rel_hum]]), 
                                   "", round(data[[mean_rel_hum]], 1))
    data[[mean_rel_hum]] <- sprintf("%6s", data[[mean_rel_hum]])
    df_8 <- data %>% tidyr::pivot_wider(id_cols = tidyselect::all_of(c(link, year)),
                                        names_from = tidyselect::all_of(month),
                                        values_from = tidyselect::all_of(mean_rel_hum),
                                        values_fill = strrep(" ", 6))
  }
  
  month_header <- paste0("Year", " ", paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                              "Aug", "Sep", "Oct", "Nov", "Dec", "ANNUAL"), 
                                            collapse = strrep(" ", 4)))
  for (i in seq_along(station_data[[station_link]])) {
    # filter data for single station
    curr_df <- data %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
    # lines to be written to txt file
    lines <- c()
    # add header lines
    lines <- append(lines, paste0("WMO Number:", strrep(" ", 28),
                                  station_data[[wmo_number]][i]))
    lines <- append(lines, paste0("Station Name:", strrep(" ", 26),
                                  station_data[[station_name]][i]))
    lines <- append(lines, paste0("Country Name:", strrep(" ", 26),
                                  station_data[[country_name]][i]))
    lines <- append(lines, paste0("Latitude (DD MM SS N/S):", strrep(" ", 15),
                                  station_data[[latitude]][i]))
    lines <- append(lines, paste0("Longitude (DDD MM SS E/W):", strrep(" ", 13),
                                  station_data[[longitude]][i]))
    lines <- append(lines, paste0("Station Height (whole meters):", strrep(" ", 9),
                                  station_data[[height_station]][i]))
    lines <- append(lines, paste0("Barometer Height (meters, to tenths):", strrep(" ", 2),
                                  station_data[[height_barometer]][i]))
    lines <- append(lines, paste0("WIGOS Station Identifier (WSI):", strrep(" ", 8),
                                  station_data[[wigos_identifier]][i]))
    if (!missing(mean_station_pressure)) {
      lines <- append(lines, "")
      lines <- append(lines, "(2) Mean Station Pressure (precision to tenths of hPa)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_2_tmp <- df_2 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_2_mean_tmp <- df_2_means
      vals <- apply(df_2_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_2_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    if (!missing(mean_sea_level_pressure)) {
      lines <- append(lines, "")
      lines <- append(lines, "(3) Mean Sea Level Pressure (precision to tenths of hPa)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_3_tmp <- df_3 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_3_mean_tmp <- df_3_means
      vals <- apply(df_3_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_3_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    if (!missing(mean_temp)) {
      lines <- append(lines, "")
      lines <- append(lines, "(4) Mean Daily Air Temperature (precision to tenths of degrees Celsius)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_4_tmp <- df_4 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_4_mean_tmp <- df_4_means
      vals <- apply(df_4_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_4_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    if (!missing(total_precip)) {
      lines <- append(lines, "")
      lines <- append(lines, "(5) Total Precipitation (precision to tenths of mm)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_5_tmp <- df_5 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_5_mean_tmp <- df_5_means
      vals <- apply(df_5_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_5_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    if (!missing(mean_max_temp)) {
      lines <- append(lines, "")
      lines <- append(lines, "(6) Mean Daily Maximum Air Temperature (precision to tenths of degree Celsius)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_6_tmp <- df_6 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_6_mean_tmp <- df_6_means
      vals <- apply(df_6_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_6_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    if (!missing(mean_min_temp)) {
      lines <- append(lines, "")
      lines <- append(lines, "(7) Mean Daily Minimum Air Temperature (precision to tenths of degree Celsius)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_7_tmp <- df_7 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_7_mean_tmp <- df_7_means
      vals <- apply(df_7_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_7_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    if (!missing(mean_rel_hum)) {
      lines <- append(lines, "")
      lines <- append(lines, "(8) Mean of the Daily Relative Humidity (whole percent)")
      lines <- append(lines, "")
      lines <- append(lines, month_header)
      lines <- append(lines, "")
      df_8_tmp <- df_8 %>% dplyr::filter(.data[[link]] == station_data[[station_link]][i])
      df_8_mean_tmp <- df_8_means
      vals <- apply(df_8_tmp, 1, function(r) paste0(r[2:14], collapse = " "))
      vals <- paste(vals, df_8_means %>% 
                      dplyr::filter(.data[[link]] == station_data[[station_link]][i]) %>%
                      dplyr::pull(mean))
      lines <- append(lines, vals)
    }
    writeLines(lines, paste0(folder, "/", station_data[[station_link]][i], "-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  }
  cat(i, "file(s) created at:", folder)
}
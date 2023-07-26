#' Convert multiple netCDF files to a single data frame
#' 
#' @description
#' This function reads multiple netCDF files from a specified path and converts them into a single data frame. It allows the user to specify the variables of interest, whether to keep the raw time values, and include metadata. Additionally, the function provides options for subsetting the data based on a boundary, specific lon/lat points, or an ID variable. The resulting data frame contains the merged data from all netCDF files.
#' 
#' @param path The path to the directory containing the netCDF files.
#' @param vars The names of the variables of interest in the netCDF files.
#' @param keep_raw_time If TRUE, keeps the raw time values as a separate column in the data frame. Default is TRUE.
#' @param include_metadata If TRUE, includes metadata information in the data frame. Default is TRUE.
#' @param boundary An optional boundary to subset the data. It should be a list with elements "lon_min", "lon_max", "lat_min", and "lat_max".
#' @param lon_points An optional vector of specific longitudes to subset the data.
#' @param lat_points An optional vector of specific latitudes to subset the data.
#' @param id_points An optional vector of specific ID points to subset the data.
#' @param show_requested_points If TRUE, includes a column indicating whether the requested lon/lat points are within the data. Default is TRUE.
#' @param great_circle_dist If TRUE, uses great circle distance calculation for subsetting based on lon/lat points. Default is TRUE.
#' @param id The name of the ID column in the merged data frame. Default is "id".
#'
#' @return The merged data frame containing the data from all netCDF files.
#' 
#' @export
#'
#' @examples
#' # Example usage
#' # path <- "path/to/netcdf/files"
#' # vars <- c("temperature", "precipitation")
#' # data <- multiple_nc_as_data_frame(path, vars)
#'
#' # Example usage with additional parameters
#' # boundary <- list(lon_min = -180, lon_max = 180, lat_min = -90, lat_max = 90)
#' # lon_points <- c(-120, -100, -80)
#' # lat_points <- c(30, 40, 50)
#' # id_points <- c("A", "B", "C")
#' # data <- multiple_nc_as_data_frame(path, vars, keep_raw_time = FALSE, include_metadata = FALSE, boundary = boundary, lon_points = lon_points, lat_points = lat_points, id_points = id_points, show_requested_points = FALSE, great_circle_dist = FALSE, id = "station_id")
#' 
multiple_nc_as_data_frame <- function(path, vars, keep_raw_time = TRUE, include_metadata = TRUE, boundary = NULL, lon_points = NULL, lat_points = NULL, id_points = NULL, show_requested_points = TRUE, great_circle_dist = TRUE, id = "id") {
  filepaths <- list.files(path = path, pattern="*\\.nc", full.names = TRUE)
  filenames <- basename(filepaths)
  nc_list <- list()
  n_files <- length(filepaths)
  is_win <- Sys.info()['sysname'] == "Windows"
  if (is_win) pb <- utils::winProgressBar(title = "Reading files", min = 0, max = n_files)
  for(i in seq_along(filepaths)) {
    nc <- ncdf4::nc_open(filename = filepaths[i])
    dat <- nc_as_data_frame(nc = nc, vars = vars, keep_raw_time = keep_raw_time, include_metadata = include_metadata, boundary = boundary, lon_points = lon_points, lat_points = lat_points, id_points = id_points, show_requested_points = show_requested_points, great_circle_dist = great_circle_dist)
    nc_list[[length(nc_list) + 1]] <- dat
    ncdf4::nc_close(nc)
    info <- paste0("Reading file ", i, " of ", n_files, " - ", round(100*i/n_files), "%")
    if (is_win) utils::setWinProgressBar(pb, value = i, title = info, label = info)
  }
  if (is_win) close(pb)
  names(nc_list) <- tools::file_path_sans_ext(filenames)
  merged_data <- dplyr::bind_rows(nc_list, .id = id)
  return(merged_data)
}
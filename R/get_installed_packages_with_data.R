#' Get Installed Packages with Data
#'
#' @description
#' This function retrieves a list of installed packages in R, optionally including only those packages that contain data.
#' 
#' @param with_data Logical value indicating whether to include only packages that contain data. Default is \code{TRUE}.
#'
#' @return A character vector containing the names of the installed packages. If \code{with_data} is \code{TRUE}, only packages with data will be included.
#' 
#' @export
#'
#' @examples
#' # Get all installed packages
#' get_installed_packages_with_data()
#'
#' # Get installed packages with data
#' get_installed_packages_with_data(with_data = TRUE)
#'
#' # Get all installed packages (including those without data)
#' get_installed_packages_with_data(with_data = FALSE)
#' 
get_installed_packages_with_data <- function(with_data = TRUE) {
  all_installed_packages <- .packages(all.available = TRUE)
  if (with_data) all_installed_packages <- unique(utils::data(package = all_installed_packages)[["results"]][,1]) 
  return(all_installed_packages)
}
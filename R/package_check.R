#' Package Check
#'
#' @description
#' This function checks the status of a specified package in the current R environment. It verifies whether the package is installed and, if so, compares the installed version with the latest version available online from CRAN.
#'
#' @param package A character string specifying the name of the package to be checked.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{status_code}{An integer indicating the package status:}
#'   \itemize{
#'     \item \code{0} - Package name not found in CRAN (incorrect spelling or not a CRAN package).
#'     \item \code{1} - Package is installed and up-to-date information is available.
#'     \item \code{2} - Package is a CRAN package but not installed.
#'     \item \code{3} - Package is installed but not found in CRAN (non-CRAN package).
#'     \item \code{4} - Package is neither installed nor found in CRAN.
#'     \item \code{5} - No internet connection available to check CRAN versions.
#'   }
#'   \item{version_comparison}{An integer comparing the installed and CRAN versions:}
#'   \itemize{
#'     \item \code{-1} - Installed version is older than the CRAN version.
#'     \item \code{0} - Installed version matches the CRAN version.
#'     \item \code{1} - Installed version is newer than the CRAN version.
#'     \item \code{NA} - Version comparison is not applicable (e.g., for non-CRAN packages).
#'   }
#'   \item{installed_version}{The installed version of the package (if available), as a character string.}
#'   \item{cran_version}{The latest version available on CRAN (if available), as a character string.}
#' }
#'
#' @export
#'
#' @examples
#' # Check package "dplyr"
#' package_check("dplyr")
#'
#' # Check package "ggplot2"
#' package_check("ggplot2")
package_check <- function(package) {
  out <- c()
  if  (!pingr::is_online())  out[[1]] <- "5" 
  else{
    if(!exists("av_packs")) {
      create_av_packs()
    }
    #CHECK the Package is a CRAN package	
    if (package %in% av_packs$Package){
      #PACKAGE IS INSTALLED 
      if (package %in% rownames(utils::installed.packages())){
        out[[1]] <- "1"
        v_machine <- as.character(utils::packageVersion(package))
        v_web <- as.character(av_packs[av_packs$Package == package, "Version"])
        out[[2]] <- utils::compareVersion(v_machine, v_web)
        out[[3]] <- v_machine
        out[[4]] <- v_web			
      }
      else  out[[1]] <- "2"	
    }
    else{
      #PACKAGE IS INSTALLED BUT NOT IN THE CRAN REPO
      if (package %in% rownames(utils::installed.packages())) out[[1]] <- "3"			
      #PACKAGE IS NOT INSTALLED AND NOT IN THE CRAN REPO
      else out[[1]] <- "4"	
    }
    
  }
  return(out)
}
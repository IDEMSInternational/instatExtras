
#' Package Check

#' @description
#' This function checks the status of a specified package in the current R environment. It verifies whether the package is installed, and if so, it compares the installed version with the latest version available online.

#' @param package A character string specifying the name of the package to be checked.

#' @return A numeric vector with the following elements:
#' \itemize{
#'   \item \code{[1]} Status code:
#'     \itemize{
#'       \item \code{0} - Package not found (incorrect spelling).
#'       \item \code{1} - Package found and installed.
#'       \item \code{2} - Package found, but not installed.
#'     }
#'   \item \code{[2]} Version comparison result:
#'     \itemize{
#'       \item \code{-1} - Installed version is older than the latest version.
#'       \item \code{0} - Installed version is the same as the latest version.
#'       \item \code{1} - Installed version is newer than the latest version.
#'     }
#'   \item \code{[3]} Installed version (if available), as a character string.
#'   \item \code{[4]} Latest version available online (if available), as a character string.
#' }

#' @export

#' @examples
#'
#' # Check package "dplyr"
#' #package_check("dplyr")
#'
#' # Check package "ggplot2"
#' #package_check("ggplot2")

package_check <- function(package) {
  out <- c()
  if (!exists("av_packs")) {
    create_av_packs()
  }
  if (package %in% rownames(utils::installed.packages())) {
    out[[1]] <- 1
    v_machine <- as.character(utils::packageVersion(package))
    v_web <- as.character(av_packs[av_packs$Package == package, "Version"])
    out[[2]] <- utils::compareVersion(v_machine, v_web)
    out[[3]] <- v_machine
    out[[4]] <- v_web
    return(out)
  } else {
    # Check if the package name is typed correctly
    if (package %in% av_packs$Package) {
      out[[1]] <- 2
      return(out)
    } else {
      # Incorrect spelling, check your spelling
      out[[1]] <- 0
      return(out)
    }
  }
}
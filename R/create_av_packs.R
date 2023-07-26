#' Create Available Packages Data Frame
#' 
#' @description
#' This function retrieves the list of available packages from the specified repository and stores it in a data frame.
#'
#' @return A data frame containing the list of available packages.
#' @export
#'
#' @examples
#' create_av_packs()
create_av_packs <- function() {
  av_packs <<- utils::available.packages(repos = "https://cran.rstudio.com/")
  av_packs <<- data.frame(av_packs)
}


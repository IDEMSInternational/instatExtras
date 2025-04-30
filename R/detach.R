#' Detach a Package If Attached
#'
#' Safely detaches a package from the R search path only if it is currently attached. 
#' This prevents errors caused by trying to detach a package that is not loaded.
#'
#' @param name A character string naming the package to be detached (e.g., `"package:ggplot2"`).
#' @param unload Logical. If `TRUE`, the namespace is also unloaded (default `TRUE`).
#'
#' @return This function is used for its side effect (detaching a package). 
#' 
#' @export
detach_package <- function(name, unload = TRUE) {
  if (name %in% search()) {
    detach(name = name, unload = unload, character.only = TRUE)
    return(paste(name, "detached successfully"))
  } else {
    return(paste(name, "not attached"))
  }
}
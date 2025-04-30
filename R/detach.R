#' Detach a Package If Attached
#'
#' Safely detaches a package from the R search path only if it is currently attached. 
#' This prevents errors caused by trying to detach a package that is not loaded.
#'
#' @param name A character string naming the package to be detached (e.g., `"package:ggplot2"`).
#' @param pos An integer indicating the position in the search path (default is 2).
#' @param unload Logical. If `TRUE`, the namespace is also unloaded (default `TRUE`).
#' @param character.only Logical. If `TRUE`, `name` is treated as a character string and not evaluated (default `FALSE`).
#' @param force Logical. If `TRUE`, forces unloading of the namespace even if other namespaces depend on it.
#'
#' @return This function is used for its side effect (detaching a package). Returns `NULL` invisibly if the package is not attached.
#' 
#' @export
detach <- function(name, pos = 2L, unload = TRUE, character.only = FALSE, force = FALSE){
  if (name %in% search()) detach(name = name, pos = pos, unload = unload, character.only = character.only, force = force)
}
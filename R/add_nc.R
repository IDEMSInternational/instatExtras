#'  Modify a path
#' 
#' @description: This function takes a path as input and returns a modified path by appending "data.nc" to it.
#' 
#' @param path A character string representing the file path.
#' 
#' @return: A character string representing the modified file path with "data.nc" appended to it.
#' 
#' @export
#' 
#' @examples
#' add_nc("my_folder/")  # Returns "my_folder/data.nc"
#' add_nc("/path/to/file.txt")  # Returns "/path/to/file.txtdata.nc"
#' add_nc("")  # Returns "data.nc"
#' 
add_nc <- function(path) {
  paste0(path, "data.nc")
}
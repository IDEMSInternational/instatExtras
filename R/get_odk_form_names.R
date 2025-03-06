#' Get ODK Form Names
#' 
#' @description
#' This function takes a username and platform as input and retrieves the names of ODK forms from the specified platform. The supported platforms are "kobo" and "ona". The function authenticates the user with the platform if the username and password are provided. Otherwise, it retrieves the form names without authentication. The function returns a character vector containing the form names.
#'
#' @param username The username for authentication with the ODK platform. Optional if authentication is not required.
#' @param platform The platform where the ODK forms are hosted. Supported values are "kobo" and "ona".
#'
#' @return A character vector containing the names of ODK forms retrieved from the specified platform.
#' 
#' @export
#'
#' @examples
#' # Get ODK form names from Kobo platform
#' # get_odk_form_names(username = "myusername", platform = "kobo")
#'
#' # Get ODK form names from Ona platform
#' # get_odk_form_names(username = "myusername", platform = "ona")
#'
#' # Get ODK form names without authentication
#' # get_odk_form_names(platform = "kobo")
#' 
#'  
get_odk_form_names = function(username, platform) {
  #TODO This should not be repeated
  if(platform == "kobo") {
    url <- "https://kc.kobotoolbox.org/api/v1/data"
  }
  else if(platform == "ona") {
    url <- "https://api.ona.io/api/v1/data"
  }
  else stop("Unrecognised platform.")
  password <- getPass(paste0(username, " password:"))
  if(!missing(username) && !missing(password)) {
    has_authentication <- TRUE
    user <- httr::authenticate(username, password)
    odk_data <- httr::GET(url, user)
  }
  else {
    has_authentication <- FALSE
    odk_data <- httr::GET(url)
  }
  forms <- httr::content(odk_data, "parse")
  form_names <- sapply(forms, function(x) x$title)
  return(form_names)
}
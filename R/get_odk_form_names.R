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
get_odk_form_names <- function(username, platform) {
  
  if (platform == "kobo") {
    url <- "https://kf.kobotoolbox.org/api/v2/assets/?asset_type=survey"
  } else if (platform == "ona") {
    url <- "https://api.ona.io/api/v1/data"
  } else {
    stop("Unrecognised platform.")
  }
  
  password <- getPass(paste0(username, " password:"))
  
  if (!missing(username) && !missing(password)) {
    user <- httr::authenticate(username, password)
    odk_data <- get_odk_http_get(url, user)
  } else {
    user <- NULL
    odk_data <- get_odk_http_get(url)
  }
  
  check_odk_status(odk_data)

  forms <- get_odk_http_content(odk_data, "parsed")
  
  # Kobo v2 is paginated, so collect all pages
  if (platform == "kobo") {
    
    all_forms <- forms$results
    next_url <- forms$`next`
    while (!is.null(next_url)) {
      odk_data <- get_odk_http_get(next_url, user)
      check_odk_status(odk_data)
      forms <- get_odk_http_content(odk_data, "parsed")
      all_forms <- c(all_forms, forms$results)
      next_url <- forms$`next`
    }
    form_names <- sapply(all_forms, function(x) x$name)
  } else {
    # ONA retains the old structure
    form_names <- sapply(forms, function(x) x$title)
  }
  return(form_names)
}

# Stops with an informative error if an ODK HTTP response was not successful
check_odk_status <- function(response, context = "ODK forms") {
  if (response$status_code != 200) {
    if (response$status_code == 401) {
      stop("Invalid username/password")
    } else {
      stop(paste0("Issue in accessing ", context, ": status_code ",
                   response$status_code, ", ",
                   nanonext::status_code(response$status_code)))
    }
  }
}

# Wrapper function for `httr::GET`
get_odk_http_get <- function(url, auth = NULL) {
  httr::GET(url, auth)
}

# Wrapper function for `httr::content`
get_odk_http_content <- function(response, type) {
  httr::content(response, type)
}
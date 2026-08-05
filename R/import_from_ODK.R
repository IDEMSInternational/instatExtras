#' Import from ODK
#' 
#' @description This function imports data from ODK platforms, such as 
#' KoboToolbox or Ona, based on the specified username, form name, and platform.
#'
#' @param username Your username (character) on the ODK platform.
#' @param form_name The name of the form (character) you want to import.
#' @param platform The ODK platform (character) you are using. Valid options
#' are "kobo" or "ona".
#'
#' @export
#' @return The imported form data as a structured object.
#'
import_from_ODK <- function(username, form_name, platform) {
  
  if (platform == "kobo") {
    url <- "https://kf.kobotoolbox.org/api/v2/assets/?asset_type=survey"
  } else if (platform == "ona") {
    url <- "https://api.ona.io/api/v1/data"
  } else {
    stop("Unrecognised platform.")
  }
  
  password <- getPass(paste0(username, " password:"))
  
  if (!missing(username) && !missing(password)) {
    has_authentication <- TRUE
    user <- httr::authenticate(username, password)
    odk_data <- get_odk_http_get(url, user)
  } else {
    has_authentication <- FALSE
    user <- NULL
    odk_data <- get_odk_http_get(url)
  }
  
  check_odk_status(odk_data)

  forms <- get_odk_http_content(odk_data, "parsed")
  
  if (platform == "kobo") {
    
    # Get all forms from the first page
    all_forms <- forms$results
    
    # Follow pagination to get all forms
    next_url <- forms$`next`
    
    while (!is.null(next_url)) {
      
      odk_data <- get_odk_http_get(next_url, user)

      check_odk_status(odk_data)

      forms <- get_odk_http_content(odk_data, "parsed")
      all_forms <- c(all_forms, forms$results)
      next_url <- forms$`next`
    }
    
    form_names <- sapply(all_forms, function(x) x$name)
    
    if (!form_name %in% form_names) {
      stop(form_name, " not found in available forms: ", 
           paste(form_names, collapse = ", "))
    }
    
    form_num <- which(form_names == form_name)
    form_id <- all_forms[[form_num]]$uid
    
    # Kobo v2 data endpoint
    data_url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/", form_id, "/data/")
    
    # Get first page of submissions
    if (has_authentication) {
      curr_form <- get_odk_http_get(data_url, user)
    } else {
      curr_form <- get_odk_http_get(data_url)
    }
    
    check_odk_status(curr_form, context = "ODK form data")

    form_data <- get_odk_http_content(curr_form, "parsed")

    # Kobo v2 submission data is paginated
    all_data <- form_data$results
    
    next_url <- form_data$`next`
    
    while (!is.null(next_url)) {
      
      if (has_authentication) {
        curr_form <- get_odk_http_get(next_url, user)
      } else {
        curr_form <- get_odk_http_get(next_url)
      }
      
      check_odk_status(curr_form, context = "ODK form data")

      form_data <- get_odk_http_content(curr_form, "parsed")
      
      all_data <- c(all_data, form_data$results)
      next_url <- form_data$`next`
    }
    
    out <- jsonlite::fromJSON(
      jsonlite::toJSON(all_data),
      flatten = TRUE
    )
    
  } else {
    
    # ONA retains the old structure
    form_names <- sapply(forms, function(x) x$title)
    
    if (!form_name %in% form_names) {
      stop(
        form_name,
        " not found in available forms: ",
        paste(form_names, collapse = ", ")
      )
    }
    
    form_num <- which(form_names == form_name)
    form_id <- forms[[form_num]]$id
    
    if (has_authentication) {
      curr_form <- get_odk_http_get(
        paste0(url, "/", form_id),
        user
      )
    } else {
      curr_form <- get_odk_http_get(
        paste0(url, "/", form_id)
      )
    }
    
    form_data <- get_odk_http_content(curr_form, "text")
    
    out <- jsonlite::fromJSON(
      form_data,
      flatten = TRUE
    )
  }
  
  return(out)
}

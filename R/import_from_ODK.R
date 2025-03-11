#'  Import from ODK
#' @description This function imports data from ODK platforms, such as KoboToolbox or Ona, based on the specified username, form name, and platform.
#' @param username Your username (character) on the ODK platform.
#' @param form_name The name of the form (character) you want to import
#' @param platform The ODK platform (character)you are using. Valid options are "kobo" or "ona".
#' 
#' @export
#'@return The imported form data as a structured object.
#'
import_from_ODK = function(username, form_name, platform) {
  if(platform == "kobo") {
    url <- "https://kc.kobotoolbox.org/api/v1/data"
  } else if(platform == "ona") {
    url <- "https://api.ona.io/api/v1/data"
  }
  else stop("Unrecognised platform.")
  password <- getPass(paste0(username, " password:"))
  if(!missing(username) && !missing(password)) {
    has_authentication <- TRUE
    user <- httr::authenticate(username, password)
    odk_data <- get_odk_http_get(url, user)
  } else {
    has_authentication <- FALSE
    odk_data <- get_odk_http_get(url)
  }
  
  forms <- get_odk_http_content(odk_data, "parse")
  if (odk_data$status_code != 200){
    if (odk_data$status_code == 401) stop("Invalid username/password")
    else stop(paste0("Issue in accessing ODK forms: status_code ", odk_data$status_code, ", ", nanonext::status_code(odk_data$status_code)))
  }
  form_names <- sapply(forms, function(x) x$title)    # get_odk_form_names_results <- get_odk_form_names(username, platform)
  # form_names <- get_odk_form_names_results[1]
  # forms <- get_odk_form_names_results[2]
  
  if(!form_name %in% form_names) stop(form_name, " not found in available forms:", paste(form_names, collapse = ", "))
  form_num <- which(form_names == form_name)
  form_id <- forms[[form_num]]$id
  
  if(has_authentication) curr_form <- get_odk_http_get(paste0(url,"/", form_id), user)
  else curr_form <- get_odk_http_get(paste0(url,"/", form_id))
  
  form_data <- get_odk_http_content(curr_form, "text")
  #TODO Look at how to convert columns that are lists
  #     maybe use tidyr::unnest
  out <- jsonlite::fromJSON(form_data, flatten = TRUE)
  return(out)
}

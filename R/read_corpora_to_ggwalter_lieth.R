#' Read and Process Corpora Data
#'
#' This function reads and processes data from the rcorpora package.
#' It handles various data types including data frames, vectors, matrices, and lists.
#'
#' @param data A data frame, list, or other object containing the data to be processed.
#' @return A data frame with the processed data, including metadata and descriptions if available.
#' @examples
#' \dontrun{
#'   read_corpora(my_data)
#' }
#' @export
read_corpora <- function(data){
  data_all <- NULL
  description <- NULL
  # check different data types that are in the rcorpora package
  # first check if it is a data frame outright. If it is, then we just need to return the data
  if (is.data.frame(data)){
    return(data)
  } 
  # If it isn't a data frame, we check each element of the `data` argument
  data_unlist <- NULL
  for (i in 1:length(data)){
    # first, check for description and metadata
    if (!is.null(names(data[i])) && names(data[i]) == "description") {
      description <- data[i][[1]]
    } else if (!is.null(names(data[i])) && names(data[i]) == "meta"){
      data_unlist[[i]] <- NULL
      # then check if the element is a vector, matrix, data frame, or list.
    } else if (class(data[[i]]) %in% c("character", "factor", "logical", "numeric", "integer")){
      data_unlist[[i]] <- data.frame(list = data[[i]])
    } else if ("matrix" %in% class(data[[i]])){
      data_unlist[[i]] <- data.frame(list = do.call(paste, c(data.frame(data[[i]]), sep="-")))
    } else if (class(data[[i]]) == "data.frame"){
      data_unlist[[i]] <- data.frame(list = data[[i]])
    } else if (class(data[[i]]) == "list"){
      if (length(data[[i]]) == 0) {
        data_unlist[[i]] <- data.frame(NA)
      } else {
        # unlist the list, to create a data frame with two elements: list name ("rowname") and value
        # if there are nested lists, the "rowname" variable combines the different lists together.
        # We want to separate these into separate variables to make the data more usable and readable.
        # We do this by `str_split_fixed`, and `gsub`.
        new_data <- tidyr::as_tibble(unlist(data), rownames = "rowname")
        split <- stringr::str_split_fixed(string=new_data$rowname, pattern=stringr::coll(pattern="."), n=Inf)
        split <- gsub("[0-9]$|[0-9][0-9]$","",split)
        # add in the separated list to the value variable, and rename the variables
        data_unlist[[i]] <- cbind(data.frame(split), value = new_data$value)
        names(data_unlist[[i]]) <- c(paste0("variable", 1:(length(data_unlist[[i]])-1)), "list")
      } # end of ifelse lists
    } # end of list
  } # end of for loop
  names(data_unlist) <- names(data[1:length(data_unlist)])
  data_all <- plyr::ldply(data_unlist, .id = "variable1")
  
  if (!is.null(description)){
    return (data.frame(description = description, data_all))
  } 
  return (data.frame(data_all))
}

#' Bind Data Frames and Remove Duplicates
#'
#' This function binds two data frames and removes duplicates from the first data frame based on the columns in the second data frame.
#'
#' @param x A data frame from which duplicates will be removed.
#' @param y A data frame containing the columns to bind into `x`.
#' @param cols A character vector of column names to use for binding.
#' @return A data frame with the bound columns and duplicates removed.
#' @examples
#' \dontrun{
#'   cbind_unique(df1, df2, c("col1", "col2"))
#' }
#' @export
cbind_unique <- function(x, y, cols){
  x <- x %>% dplyr::select(c(setdiff(names(x), cols)))
  x <- dplyr::bind_cols(x = x, y = dplyr::select(y, tidyselect::all_of(cols)))
}

#' View Object Data
#'
#' Displays the given object in a specified format. If no format is provided, the object is printed.
#'
#' @param object The object to be displayed.
#' @param object_format The format in which to display the object ("image", "text", "html"). Defaults to NULL.
#' @return A file name if the object is saved to a file, otherwise NULL.
#' @examples
#' \dontrun{
#'   view_object_data(my_object, "text")
#' }
#' @export
view_object_data <- function(object, object_format = NULL) {
  file_name <- ""
  if (identical(object_format, "image")) {
    file_name <- view_graph_object(object)
  } else if (identical(object_format, "text")) {
    file_name <- view_text_object(object)
  } else if (identical(object_format, "html")) {
    file_name <- view_html_object(object)
  }  else{
    print(object)
  }
  return(file_name)
}

#' View Object
#'
#' Displays the object stored in a data book object.
#'
#' @param data_book_object A data book object containing the object to be displayed and its format.
#' @return The file name if the object is saved to a file, otherwise NULL.
#' @examples
#' \dontrun{
#'   view_object(my_data_book)
#' }
#' @export
view_object <- function(data_book_object) {
  return(
    view_object_data(
      object = data_book_object$object,
      object_format = data_book_object$object_format
    )
  )
}

#' View Graph Object
#'
#' Displays the graph object in the R viewer. If the viewer is not available, saves the object as a file in the temporary folder.
#'
#' @param graph_object The graph object to be displayed.
#' @return The graph object or the file name if saved as an image.
#' @examples
#' \dontrun{
#'   view_graph_object(my_graph)
#' }
#' @export
view_graph_object <- function(graph_object){
  #get object class names
  object_class_names <- class(graph_object)
  
  #if there is a viewer, like in the case of RStudio then just print the object
  #this check is primarily meant to make this function work in a similar manner when run outside R-Instat
  r_viewer <- base::getOption("viewer")
  if (!is.null(r_viewer)) {
    #TODO. When print command is called in R-Studio, a temp file is automatically created
    #Investigate how that can be done in R-Instat
    #as of 07/09/2022 just return the object. Important for RStudio to display the object
    if ("grob" %in% object_class_names){
      #for grob objects draw them first
      grid::grid.draw(graph_object)
    }
    return(graph_object)
  }
  
  
  #get a unique temporary file name from the tempdir path
  file_name <- tempfile(pattern = "viewgraph", fileext = ".png")
  
  #save the object as a graph file depending on the object type
  grDevices::png(file = file_name, width = 4000, height = 4000, res = 500)
  if ("grob" %in% object_class_names) {
    grid::grid.draw(graph_object)
  }else{
    print(graph_object)
  }
  dev.off() #todo. use graphics.off() which one is better?
  
  
  #todo. should we use respective package "convenience" functions to save the objects as image files depending on the class names?
  #investigate if it will help with resolution and scaling?
  # if ("ggplot" %in% object_class_names) {
  # } else if ("ggmultiplot" %in% object_class_names) {
  # } else if ("openair" %in% object_class_names) {
  # } else if ("ggsurvplot" %in% object_class_names) {
  # } else if ("recordedplot" %in% object_class_names) {
  # }
  
  message("R viewer not detected. File saved in location ", file_name)
  return(file_name)
}

#' View Text Object
#'
#' Displays the text object in the R viewer. If the viewer is not available, saves the object as a file in the temporary folder.
#'
#' @param text_object The text object to be displayed.
#' @return The text object or the file name if saved as a text file.
#' @examples
#' \dontrun{
#'   view_text_object(my_text)
#' }
#' @export
view_text_object <- function(text_object){
  #if there is a viewer, like in the case of RStudio then just print the object
  #this check is primarily meant to make this function work in a similar manner when run outside R-Instat
  r_viewer <- base::getOption("viewer")
  if (!is.null(r_viewer)) {
    #TODO. When print command is called in R-Studio, a temp file is
    #automatically created. Investigate how that can be done in R-Instat
    #as of 07/09/2022 just return output. Important for RStudio to display the object
    return(utils::capture.output(text_object))
  }
  
  
  #get a unique temporary file name from the tempdir path
  file_name <- tempfile(pattern = "viewtext", fileext = ".txt")
  
  #todo. should we use respective package "convenience" functions to save the objects as text files depending on the class names
  #get object class names
  #object_class_names <- class(text_object)
  
  #save the object as a text file 
  utils::capture.output(text_object, file = file_name)
  
  message("R viewer not detected. File saved in location ", file_name)
  return(file_name)
}

#' View HTML Object
#'
#' Displays the HTML object in the R viewer. If the viewer is not available, saves the object as a file in the temporary folder.
#'
#' @param html_object The HTML object to be displayed.
#' @return The HTML object or the file name if saved as an HTML file.
#' @examples
#' \dontrun{
#'   view_html_object(my_html)
#' }
#' @export
view_html_object <- function(html_object) {
  # Check if html_object is a list and has more than one element
  if (is.list(html_object) && all(sapply(html_object, class) == class(html_object[[1]]))) {
    file_names <- vector("list", length(html_object))
    for (i in seq_along(html_object)) {
      # If html_object is a list with multiple elements of the same class, 
      # use a for loop to process each element
      file_names[[i]] <- process_html_object(html_object[[i]])
    }
    return(file_names)
  }
  
  # Process the html_object
  return(process_html_object(html_object))
}

#' Process Individual HTML Object
#'
#' Displays the HTML object in the R viewer. If the viewer is not available, saves the object as a file in the temporary folder.
#'
#' @param html_object The HTML object to be processed.
#' @return The HTML object or the file name if saved as an HTML file.
#' @examples
#' \dontrun{
#'   process_html_object(my_html_object)
#' }
#' @export
process_html_object <- function(html_object) {
  #if there is a viewer, like in the case of RStudio then just print the object
  #this check is primarily meant to make this function work in a similar manner when run outside R-Instat
  r_viewer <- base::getOption("viewer")
  if (!is.null(r_viewer)) {
    #When print command is called in R-Studio, a temp file is
    #automatically created. TODO. Investigate how that can be done in R-Instat. 
    #as of 07/09/2022 just return the object. Important for RStudio to display the object
    return(html_object)
  }
  
  # Get a unique temporary file name from the tempdir path
  file_name <- tempfile(pattern = "viewhtml", fileext = ".html")
  
  # Get a vector of available class names
  object_class_names <- class(html_object)
  
  # Save the object as an HTML file depending on the object type
  if ("htmlwidget" %in% object_class_names) {
    #Note. When selfcontained is set to True 
    #a "Saving a widget with selfcontained = TRUE requires pandoc" error is thrown in R-Instat
    #when saving an rpivotTable 
    #TODO. Investigate how to solve it then. 
    htmlwidgets::saveWidget(html_object, file = file_name, selfcontained = FALSE)
  } else if ("sjTable" %in% object_class_names) {
    #"sjTable" objects are not compatible with "htmlwidgets" package. So they have to be saved differently
    #"sjplot" package produces "sjTable" objects 
    html_object$file = file_name
    #TODO. Is there any other function that can save an sjTable to a html file?
    print(html_object)
  } else if ("gt_tbl" %in% object_class_names) {
    #"gt table" objects are not compatible with "htmlwidgets" package. So they have to be saved differently.
    #"mmtable2" package produces "gt_tbl" objects 
    gt::gtsave(html_object, filename = file_name)
  }
  
  message("R viewer not detected. File saved in location ", file_name)
  return(file_name)
}

#' Check Graph
#'
#' Records the plot if `graph_object` is NULL and returns the graph object of class "recordedplot". Applicable to base graphs only.
#'
#' @param graph_object The graph object to be checked.
#' @return The recorded plot object or NULL if an error occurs.
#' @examples
#' \dontrun{
#'   check_graph(my_graph)
#' }
#' @export
check_graph <- function(graph_object){
  
  out <- graph_object
  
  if (is.null(out)) {
    out <- tryCatch({
      message("Recording plot")
      recordPlot()
    },
    error = function(cond) {
      message("Graph object does not exist:")
      message(cond)
      # Choose a return value in case of error
      return(NULL)
    },
    warning = function(cond) {
      message("Warning message:")
      message(cond)
      return(NULL)
    },
    finally = {
      message("Plot recorded")
    })
  }
  
  return(out)
}

#' Get Data Book Output Object Names
#'
#' Retrieves the names of output objects in a data book list.
#'
#' @param output_object_list A list of output objects.
#' @param object_type_label An optional label to filter the objects by type. Defaults to NULL.
#' @param excluded_items A character vector of items to exclude from the result. Defaults to an empty vector.
#' @param as_list Logical indicating whether to return the result as a list. Defaults to FALSE.
#' @param list_label An optional label for the list if `as_list` is TRUE. Defaults to NULL.
#' @return A character vector or list of object names.
#' @examples
#' \dontrun{
#'   get_data_book_output_object_names(my_objects)
#' }
#' @export
get_data_book_output_object_names <- function(output_object_list,
                                              object_type_label = NULL, 
                                              excluded_items = c(), 
                                              as_list = FALSE, 
                                              list_label = NULL){
  
  if(is.null(object_type_label)){
    out <- names(output_object_list)
  }else{ 
    out <- names(output_object_list)[sapply(output_object_list, function(x) any( identical(x$object_type_label, object_type_label) ))]
  }
  
  if(length(out) == 0){
    return(out)
  } 
  
  if(length(excluded_items) > 0) {
    #get indices of items to exclude
    excluded_indices <- which(out %in% excluded_items)
    
    #remove the excluded items from the list
    if(length(excluded_indices) > 0){
      out <- out[-excluded_indices]
    }
    
  }
  
  if(as_list) {
    #convert the character vector list
    lst <- list()
    if(!is.null(list_label)){
      lst[[list_label]] <- out
    }else{
      lst <- as.list(out)
    }
    
    return(lst)
  }else{
    #return as a character vector
    return(out)
  }
  
}

#' Get Vignette
#'
#' Retrieves the vignette information for a specified package.
#'
#' @param package The name of the package. Defaults to NULL.
#' @param lib.loc The library location. Defaults to NULL.
#' @param all Logical indicating whether to list all available vignettes. Defaults to TRUE.
#' @return A character vector with the vignette information.
#' @examples
#' \dontrun{
#'   get_vignette("dplyr")
#' }
#' @export
get_vignette <- function (package = NULL, lib.loc = NULL, all = TRUE) 
{   
  oneLink <- function(s) {
    if (length(s) == 0L) 
      return(character(0L))
    title <- s[, "Title"]
    if (port > 0L) 
      prefix <- sprintf("/library/%s/doc", pkg)
    else prefix <- sprintf("file://%s/doc", s[, "Dir"])
    src <- s[, "File"]
    pdf <- s[, "PDF"]
    rcode <- s[, "R"]
    pdfext <- sub("^.*\\.", "", pdf)
    sprintf("  <li>%s  -  \n    %s  \n    %s  \n    %s \n  </li>\n", 
            title, ifelse(nzchar(pdf), sprintf("<a href='%s/%s'>%s</a>&nbsp;", 
                                               prefix, pdf, toupper(pdfext)), ""), sprintf("<a href='%s/%s'>source</a>&nbsp;", 
                                                                                           prefix, src), ifelse(nzchar(rcode), sprintf("<a href='%s/%s'>R code</a>&nbsp;", 
                                                                                                                                       prefix, rcode), ""))
  }
  
  port <- tools::startDynamicHelp(NA)
  file <- tempfile("Rvig.", fileext = ".html")
  print(file)
  sink(file = file, type = "output")
  vinfo <- tools::getVignetteInfo(package, lib.loc, all)
  pkgs <- unique(vinfo[, "Package"])
  db <- lapply(pkgs, function(p) vinfo[vinfo[, "Package"] == 
                                         p, , drop = FALSE])
  names(db) <- pkgs
  attr(db, "call") <- sys.call()
  attr(db, "footer") <- if (all) 
    ""
  else sprintf(gettext("Use <code> %s </code> \n to list the vignettes in all <strong>available</strong> packages."), 
               "browseVignettes(all = TRUE)")
  if (port > 0L) 
    css_file <- "/doc/html/R.css"
  else css_file <- file.path(R.home("doc"), "html", 
                             "R.css")
  cat(sprintf("<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>\n<html>\n<head>\n<title>R Vignettes</title>\n<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>\n<meta name='viewport' content='width=device-width, initial-scale=1.0, user-scalable=yes' />\n<link rel='stylesheet' type='text/css' href='%s'>\n</head><body><div class='container'>\n", 
              css_file))
  
  cat(sprintf("<center><h3>Vignettes found by <code><q>%s</q></code></h3></center>", 
              deparse1(attr(db, "call"))))
  cat("<div class=\"vignettes\">")
  for (pkg in names(db)) {
    cat(sprintf("<h4>Vignettes in package <code>%s</code></h4>\n", 
                pkg))
    cat("<ul>\n")
    links <- oneLink(db[[pkg]])
    cat(paste(links), collapse = "\n")
    cat("\n</ul>\n")
  }
  cat("</div>")
  sink()
  if (port > 0L){
    return(sprintf("http://127.0.0.1:%d/session/%s", 
                   port, basename(file)))}
  else return(sprintf("file://%s", file))
}

#' Cumulative Inventory
#'
#' Adds a cumulative count of missing values by period (and station if specified) to the data.
#'
#' @param data The input data frame.
#' @param station An optional column name indicating the station. Defaults to NULL.
#' @param from The start period column name.
#' @param to The end period column name.
#' @return A data frame with added cumulative counts.
#' @examples
#' \dontrun{
#'   cumulative_inventory(my_data, "station", "start_date", "end_date")
#' }
#' @export
cumulative_inventory <- function(data, station = NULL, from, to){
  if (is.null(station)){
    data <- data %>%
      dplyr::group_by(.data[[from]], .data[[to]]) %>%
      dplyr::mutate(cum=dplyr::n())
    data <- data %>%
      dplyr::group_by(.data[[from]]) %>%
      dplyr::mutate(cum1 = dplyr::n()) %>% 
      dplyr::mutate(cum1 = ifelse(cum == cum1, # are they all in the same period?
                                  yes = cum,  
                                  no = ifelse(cum == max(cum),
                                              cum,
                                              max(cum) + 0.5)))
  } else {
    data <- data %>%
      dplyr::group_by(.data[[station]], .data[[from]], .data[[to]]) %>%
      dplyr::mutate(cum=dplyr::n())
    data <- data %>%
      dplyr::group_by(.data[[station]], .data[[from]]) %>%
      dplyr::mutate(cum1 = dplyr::n()) %>% 
      dplyr::mutate(cum1 = ifelse(cum == cum1, # are they all in the same period?
                                  yes = cum,  
                                  no = ifelse(cum == max(cum),
                                              cum,
                                              max(cum) + 0.5)))
  }
  return(data)
}

#' Get Row Headers with Text
#'
#' Retrieves the row headers of a data frame where the specified column contains the search text.
#'
#' @param data The input data frame.
#' @param column The name of the column to search.
#' @param searchText The text to search for.
#' @param ignore_case Logical indicating whether to ignore case. Defaults to TRUE.
#' @param use_regex Logical indicating whether to use regular expressions. Defaults to FALSE.
#' @return A character vector of row headers where the search text is found.
#' @examples
#' \dontrun{
#'   getRowHeadersWithText(my_data, "column1", "search text", TRUE, FALSE)
#' }
#' @export
getRowHeadersWithText <- function(data, column, searchText, ignore_case, use_regex) {
  if(use_regex){
    # Find the rows that match the search text using regex
    matchingRows <- stringr::str_detect(data[[column]], stringr::regex(searchText, ignore_case = ignore_case))
  }else if (is.na(searchText)){
    matchingRows <- apply(data[, column, drop = FALSE], 1, function(row) any(is.na(row)))
  }else{
    matchingRows <- grepl(searchText, data[[column]], ignore.case = ignore_case)
  }
  # Get the row headers where the search text is found
  rowHeaders <- rownames(data)[matchingRows]
  
  # Return the row headers
  return(rowHeaders)
}

#' Convert Character to List of Numeric Vectors
#'
#' Converts a character string to a list of numeric vectors.
#'
#' @param x A character string to be converted.
#' @return A list of numeric vectors.
#' @examples
#' \dontrun{
#'   convert_to_list("c(1,2,3)")
#' }
#' @export
convert_to_list <- function(x) {
  if (grepl("^c\\(", x)) {
    x <- gsub("^c\\(|\\)$", "", x)  # Remove 'c(' and ')'
    return(as.numeric(unlist(strsplit(x, ","))))
  } else if (grepl(":", x)) {
    x <- gsub(":", ",", x, fixed = TRUE)  # Replace ':' with ','
    return(as.numeric(unlist(strsplit(x, ","))))
  } else {
    return(as.numeric(x))
  }
}

#' Get Example
#'
#' Retrieves and displays the example code for a specified topic from a package.
#'
#' @param topic The topic for which to retrieve the example.
#' @param package The name of the package containing the topic. Defaults to NULL.
#' @param lib.loc The library location. Defaults to NULL.
#' @param character.only Logical indicating whether the topic is specified as a character string. Defaults to TRUE.
#' @param give.lines Logical indicating whether to return the example code as lines of text. Defaults to FALSE.
#' @param local Logical indicating whether to evaluate the example locally. Defaults to FALSE.
#' @param echo Logical indicating whether to echo the example code. Defaults to TRUE.
#' @param verbose Logical indicating whether to print verbose output. Defaults to the value of the "verbose" option.
#' @param setRNG Logical indicating whether to set the random number generator. Defaults to FALSE.
#' @param ask Logical indicating whether to ask before evaluating the example. Defaults to the value of the "example.ask" option.
#' @param prompt.prefix A prefix for the prompt. Defaults to an abbreviated version of the topic.
#' @param run.dontrun Logical indicating whether to run examples marked with `\dontrun`. Defaults to FALSE.
#' @param run.donttest Logical indicating whether to run examples marked with `\donttest`. Defaults to the value of `interactive()`.
#' @return The example code as a character string if `give.lines` is TRUE, otherwise prints the example code.
#' @examples
#' \dontrun{
#'   getExample("filter", "dplyr")
#' }
#' @export
getExample <- function (topic, package = NULL, lib.loc = NULL, character.only = TRUE, give.lines = FALSE, local = FALSE, echo = TRUE, verbose = getOption("verbose"), setRNG = FALSE, ask = getOption("example.ask"), prompt.prefix = abbreviate(topic, 6), run.dontrun = FALSE, run.donttest = interactive()) {
  if (!character.only) {
    topic <- substitute(topic)
    if (!is.character(topic))
      topic <- deparse(topic)[1L]
  }
  pkgpaths <- find.package(package, lib.loc, verbose = verbose)
  file <- utils:::index.search(topic, pkgpaths, firstOnly = TRUE)
  if (!length(file)) {
    warning(gettextf("no help found for %s", sQuote(topic)),
            domain = NA)
    return(character())
  }
  if (verbose)
    cat("Found file =", sQuote(file), "\n")
  packagePath <- dirname(dirname(file))
  pkgname <- basename(packagePath)
  lib <- dirname(packagePath)
  tf <- tempfile("Rex")
  tools::Rd2ex(utils:::.getHelpFile(file), tf, commentDontrun = !run.dontrun,
               commentDonttest = !run.donttest)
  if (!file.exists(tf)) {
    if (give.lines)
      return(character())
    warning(gettextf("%s has a help file but no examples",
                     sQuote(topic)), domain = NA)
    return(character())
  }
  on.exit(unlink(tf))
  example_text <- readLines(tf)
  example_text <- paste(example_text, collapse = "\n")
  if (give.lines) {
    return(example_text)
  }
  if (echo) {
    cat(example_text)
  }
  return(example_text)
}

#' Calculate Evaporation for Water Balance
#'
#' Calculates the evaporation based on water balance, fraction of capacity, and other parameters.
#'
#' @param water_balance The current water balance.
#' @param frac The fraction of capacity.
#' @param capacity The total capacity.
#' @param evaporation_value The value of evaporation.
#' @param rain The amount of rain.
#' @return The calculated evaporation.
#' @examples
#' \dontrun{
#'   WB_evaporation(100, 0.5, 200, 10, 5)
#' }
#' @export
WB_evaporation <- function(water_balance, frac, capacity, evaporation_value, rain){
  if (water_balance >= frac*capacity){
    evaporation <- evaporation_value
  } else {
    if (rain == 0){
      evaporation <- evaporation_value * ((water_balance)/(frac*capacity))
    } else {
      if (water_balance < frac*capacity){
        if (rain > evaporation_value){
          evaporation <- evaporation_value
        } else {
          evaporation <- evaporation_value * ((water_balance)/(frac*capacity))
          evaporation <- evaporation + ((evaporation_value - evaporation)*(rain/evaporation_value))
        }
      } else {
        evaporation <- evaporation_value
      }
    }
  }
  return(evaporation)
}

#' Write Weather Data to File
#'
#' Writes weather data to a text file with specified missing value codes.
#'
#' @param year A vector of years.
#' @param month A vector of months.
#' @param day A vector of days.
#' @param rain A vector of rainfall values.
#' @param mn_tmp A vector of minimum temperatures.
#' @param mx_tmp A vector of maximum temperatures.
#' @param missing_code The code to use for missing values.
#' @param output_file The name of the output file.
#' @return None. Writes the data to a file.
#' @examples
#' \dontrun{
#'   write_weather_data(2020, 1:12, 1:31, rain_data, min_temp, max_temp, -99, "weather.txt")
#' }
#' @export
write_weather_data <- function(year, month, day, rain, mn_tmp, mx_tmp, missing_code, output_file) {
  # Create a data frame with the provided inputs
  weather_data <- data.frame(year = year,
                             month = month,
                             day = day,
                             rain = rain,
                             mn_tmp = mn_tmp,
                             mx_tmp = mx_tmp)
  
  # Replace missing values with the specified code
  weather_data[is.na(weather_data)] <- missing_code
  
  # Write the data frame to a text file
  write.table(weather_data, file = output_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  cat("Weather data has been written to", output_file, "\n")
}

#' Prepare Data for Walter-Lieth Plot
#'
#' Prepares data for Walter-Lieth climate diagrams.
#'
#' @param data The input data frame.
#' @param month The column name for months.
#' @param tm_min The column name for minimum temperature.
#' @param ta_min The column name for average temperature.
#' @return A list of data frames and plots prepared for Walter-Lieth climate diagrams.
#' @examples
#' \dontrun{
#'   prepare_walter_lieth(my_data, "Month", "MinTemp", "AvgTemp")
#' }
#' @export
prepare_walter_lieth <- function(data, month, tm_min, ta_min){
  dat_long_int <- NULL
  for (j in seq(nrow(data) - 1)) {
    intres <- NULL
    for (i in seq_len(ncol(data))) {
      if (is.character(data[j, i]) | is.factor(data[j, i])) {
        val <- as.data.frame(data[j, i])
      }
      else {
        interpol <- approx(x = data[c(j, j + 1), "indrow"],
                           y = data[c(j, j + 1), i],
                           n = 50)
        val <- as.data.frame(interpol$y)
      }
      names(val) <- names(data)[i]
      intres <- dplyr::bind_cols(intres, val)
    }
    dat_long_int <- dplyr::bind_rows(dat_long_int, intres)
  }
  dat_long_int$interpolate <- TRUE
  dat_long_int[[month]] <- ""
  data$interpolate <- FALSE
  dat_long_int <- dat_long_int[!dat_long_int$indrow %in% data$indrow, ]
  dat_long_end <- dplyr::bind_rows(data, dat_long_int)
  dat_long_end <- dat_long_end[order(dat_long_end$indrow), ]
  dat_long_end <- dat_long_end[dat_long_end$indrow >= 0 & dat_long_end$indrow <= 12, ]
  dat_long_end <- tibble::as_tibble(dat_long_end)
  
  getpolymax <- function(x, y, y_lim) {
    initpoly <- FALSE
    yres <- NULL
    xres <- NULL
    for (i in seq_len(length(y))) {
      lastobs <- i == length(x)
      if (y[i] > y_lim[i]) {
        if (isFALSE(initpoly)) {
          xres <- c(xres, x[i])
          yres <- c(yres, y_lim[i])
          initpoly <- TRUE
        }
        xres <- c(xres, x[i])
        yres <- c(yres, y[i])
        if (lastobs) {
          xres <- c(xres, x[i], NA)
          yres <- c(yres, y_lim[i], NA)
        }
      }
      else {
        if (initpoly) {
          xres <- c(xres, x[i - 1], NA)
          yres <- c(yres, y_lim[i - 1], NA)
          initpoly <- FALSE
        }
      }
    }
    poly <- tibble::tibble(x = xres, y = yres)
    return(poly)
  }
  getlines <- function(x, y, y_lim) {
    yres <- NULL
    xres <- NULL
    ylim_res <- NULL
    for (i in seq_len(length(y))) {
      if (y[i] > y_lim[i]) {
        xres <- c(xres, x[i])
        yres <- c(yres, y[i])
        ylim_res <- c(ylim_res, y_lim[i])
      }
    }
    line <- tibble::tibble(x = xres, y = yres, ylim_res = ylim_res)
    return(line)
  }
  prep_max_poly <- getpolymax(x = dat_long_end$indrow, y = pmax(dat_long_end$pm_reesc, 
                                                                50), y_lim = rep(50, length(dat_long_end$indrow)))
  tm_max_line <- getlines(x = dat_long_end$indrow, y = dat_long_end$tm, 
                          y_lim = dat_long_end$pm_reesc)
  pm_max_line <- getlines(x = dat_long_end$indrow, y = pmin(dat_long_end$pm_reesc, 
                                                            50), y_lim = dat_long_end$tm)
  dat_real <- dat_long_end[dat_long_end$interpolate == FALSE, 
                           c("indrow", ta_min)]
  x <- NULL
  y <- NULL
  for (i in seq_len(nrow(dat_real))) {
    if (dat_real[i, ][[ta_min]] < 0) {
      x <- c(x, NA, rep(dat_real[i, ]$indrow - 0.5, 2), 
             rep(dat_real[i, ]$indrow + 0.5, 2), NA)
      y <- c(y, NA, -3, 0, 0, -3, NA)
    }
    else {
      x <- c(x, NA)
      y <- c(y, NA)
    }
  }
  probfreeze <- tibble::tibble(x = x, y = y)
  rm(dat_real)
  dat_real <- dat_long_end[dat_long_end$interpolate == FALSE, 
                           c("indrow", tm_min)]
  x <- NULL
  y <- NULL
  for (i in seq_len(nrow(dat_real))) {
    if (dat_real[i, ][[tm_min]] < 0) {
      x <- c(x, NA, rep(dat_real[i, ]$indrow - 0.5, 2), 
             rep(dat_real[i, ]$indrow + 0.5, 2), NA)
      y <- c(y, NA, -3, 0, 0, -3, NA)
    }
    else {
      x <- c(x, NA)
      y <- c(y, NA)
    }
  }
  surefreeze <- tibble::tibble(x = x, y = y)
  return_list <- list(dat_long_end,
                      tm_max_line,
                      pm_max_line,
                      prep_max_poly,
                      probfreeze,
                      surefreeze)
  names(return_list) <- c("dat_long_end", "tm_max_line", "pm_max_line",
                          "prep_max_poly", "prob_freeze", "surefreeze")
  return(return_list)
}

#' Generate Walter-Lieth Plot
#'
#' Generates Walter-Lieth climate diagrams using ggplot2.
#'
#' @param data The input data frame.
#' @param month The column name for months.
#' @param station An optional column name for station. Defaults to NULL.
#' @param p_mes The column name for precipitation measurements.
#' @param tm_max The column name for maximum temperature.
#' @param tm_min The column name for minimum temperature.
#' @param ta_min The column name for average temperature.
#' @param station_name The name of the station. Defaults to an empty string.
#' @param alt The altitude. Defaults to NA.
#' @param per The period. Defaults to NA.
#' @param pcol The color for precipitation lines. Defaults to "#002F70".
#' @param tcol The color for temperature lines. Defaults to "#ff0000".
#' @param pfcol The fill color for probable freeze areas. Defaults to "#9BAEE2".
#' @param sfcol The fill color for sure freeze areas. Defaults to "#3C6FC4".
#' @param shem Logical indicating whether the station is in the southern hemisphere. Defaults to FALSE.
#' @param p3line Logical indicating whether to plot the precipitation/3 line. Defaults to FALSE.
#' @param ... Additional arguments to be passed to ggplot2 functions.
#' @return A ggplot2 object with the Walter-Lieth climate diagram.
#' @examples
#' \dontrun{
#'   ggwalter_lieth(my_data, "Month", NULL, "Precipitation", "MaxTemp", "MinTemp", "AvgTemp")
#' }
#' @export
ggwalter_lieth <- function (data, month, station = NULL, p_mes, tm_max, tm_min, ta_min, station_name = "", 
                            alt = NA, per = NA, pcol = "#002F70", 
                            tcol = "#ff0000", pfcol = "#9BAEE2", sfcol = "#3C6FC4", 
                            shem = FALSE, p3line = FALSE, ...) 
{
  
  # Preprocess data with vectorised operations
  data <- data %>%
    dplyr::mutate(tm = (.data[[tm_max]] + .data[[tm_min]]) / 2,
                  pm_reesc = dplyr::if_else(.data[[p_mes]] < 100, .data[[p_mes]] * 0.5, .data[[p_mes]] * 0.05 + 45),
                  p3line = .data[[p_mes]] / 3) %>%
    dplyr::mutate(across(.data[[month]], ~ forcats::fct_expand(.data[[month]], ""))) %>%
    dplyr::arrange(.data[[month]])
  # do this for each station, if we have a station
  if (!is.null(station)){
    data <- data %>% group_by(!!sym(station))
  }
  data <- data %>%
    group_modify(~{
      # Add dummy rows at the beginning and end for each group
      .x <- bind_rows(.x[nrow(.x), , drop = FALSE], .x, .x[1, , drop = FALSE])
      # Clear month value for the dummy rows
      .x[c(1, nrow(.x)), which(names(.x) == data[[month]])] <- ""
      # Add an index column for plotting or further transformations
      .x <- cbind(indrow = seq(-0.5, 12.5, 1), .x)
      .x
    })
  
  if (!is.null(station)){
    data <- data %>% ungroup()
  }
  data <- data.frame(data)
  
  # split by station
  if (is.null(station)){
    data_list <- prepare_walter_lieth(data, month, tm_min, ta_min)
    # data things
    dat_long_end <- data_list$dat_long_end
    tm_max_line <- data_list$tm_max_line
    pm_max_line <- data_list$pm_max_line
    prep_max_poly <- data_list$prep_max_poly
    probfreeze <- data_list$prob_freeze
    surefreeze <- data_list$surefreeze
  } else {
    results <-
      map(.x = unique(data[[station]]),
          .f = ~{filtered_data <- data %>% filter(!!sym(station) == .x)
          prepare_walter_lieth(filtered_data, month, tm_min, ta_min)})
    # Function to bind rows for a specific sub-element across all main elements
    n <- length(results)
    m <- length(results[[1]])
    station_name <- unique(data[[station]])
    binds <- NULL
    combined <- NULL
    for (j in 1:m){
      for (i in 1:n) { # for each station data set
        binds[[i]] <- results[[i]][[j]] %>% mutate(!!sym(station) := station_name[i])
      }
      combined[[j]] <- do.call(rbind, binds) # Combine all the sub-elements row-wise
    }
    # data things
    dat_long_end <- combined[[1]]
    tm_max_line <- combined[[2]]
    pm_max_line <- combined[[3]]
    prep_max_poly <- combined[[4]]
    probfreeze <- combined[[5]]
    surefreeze <- combined[[6]]
  }
  
  # data frame pretty things ------------------------------------------------------
  ticks <- data.frame(x = seq(0, 12), ymin = -3, ymax = 0)
  month_breaks <- dat_long_end[dat_long_end[[month]] != "", ]$indrow
  month_labs <- dat_long_end[dat_long_end[[month]] != "", ][[month]]
  
  ymax <- max(60, 10 * floor(max(dat_long_end$pm_reesc)/10) + 10)
  ymin <- min(-3, min(dat_long_end$tm))
  range_tm <- seq(0, ymax, 10)
  if (ymin < -3) {
    ymin <- floor(ymin/10) * 10
    range_tm <- seq(ymin, ymax, 10)
  }
  templabs <- paste0(range_tm)
  templabs[range_tm > 50] <- ""
  range_prec <- range_tm * 2
  range_prec[range_tm > 50] <- range_tm[range_tm > 50] * 20 - 900
  preclabs <- paste0(range_prec)
  preclabs[range_tm < 0] <- ""
  
  wandlplot <- ggplot2::ggplot() + ggplot2::geom_line(data = dat_long_end, 
                                                      aes(x = .data$indrow, y = .data$pm_reesc), color = pcol) + 
    ggplot2::geom_line(data = dat_long_end, aes(x = .data$indrow, 
                                                y = .data$tm), color = tcol)
  if (nrow(tm_max_line > 0)) {
    wandlplot <- wandlplot + ggplot2::geom_segment(aes(x = .data$x, 
                                                       y = .data$ylim_res, xend = .data$x, yend = .data$y), 
                                                   data = tm_max_line, color = tcol, alpha = 0.2)
  }
  if (nrow(pm_max_line > 0)) {
    wandlplot <- wandlplot + ggplot2::geom_segment(aes(x = .data$x, 
                                                       y = .data$ylim_res, xend = .data$x, yend = .data$y), 
                                                   data = pm_max_line, color = pcol, alpha = 0.2)
  }
  if (p3line) {
    wandlplot <- wandlplot + ggplot2::geom_line(data = dat_long_end, 
                                                aes(x = .data$indrow, y = .data$p3line), color = pcol)
  }
  if (max(dat_long_end$pm_reesc) > 50) {
    wandlplot <- wandlplot + ggplot2::geom_polygon(data = prep_max_poly, aes(x, y),
                                                   fill = pcol)
  }
  if (min(dat_long_end[[ta_min]]) < 0) {
    wandlplot <- wandlplot + ggplot2::geom_polygon(data = probfreeze, aes(x = x, y = y),
                                                   fill = pfcol, colour = "black")
  }
  if (min(dat_long_end[[tm_min]]) < 0) {
    wandlplot <- wandlplot + geom_polygon(data = surefreeze, aes(x = x, y = y),
                                          fill = sfcol, colour = "black")
  }
  wandlplot <- wandlplot + geom_hline(yintercept = c(0, 50), 
                                      linewidth = 0.5) +
    geom_segment(data = ticks, aes(x = x, xend = x, y = ymin, yend = ymax)) +
    scale_x_continuous(breaks = month_breaks, name = "", labels = month_labs, expand = c(0, 0)) + 
    scale_y_continuous("C", limits = c(ymin, ymax), labels = templabs, 
                       breaks = range_tm, sec.axis = dup_axis(name = "mm", labels = preclabs))
  wandlplot <- wandlplot +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line.x.bottom = element_blank(), 
                   axis.title.y.left = element_text(angle = 0, 
                                                    vjust = 0.9, size = 10, colour = tcol,
                                                    margin = unit(rep(10, 4), "pt")),
                   axis.text.x.bottom = element_text(size = 10), 
                   axis.text.y.left = element_text(colour = tcol, size = 10), 
                   axis.title.y.right = element_text(angle = 0, vjust = 0.9, 
                                                     size = 10, colour = pcol,
                                                     margin = unit(rep(10, 4), "pt")),
                   axis.text.y.right = element_text(colour = pcol, size = 10))
  
  if (!is.null(station)){
    wandlplot <- wandlplot + facet_wrap(station)
  }
  
  return(wandlplot)
}
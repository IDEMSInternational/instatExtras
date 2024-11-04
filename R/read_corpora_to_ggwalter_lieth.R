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
  index.search < -utils::getFromNamespace("index.search", "utils")
  `.getHelpFile` <- utils::getFromNamespace(".getHelpFile", "utils")
  if (!character.only) {
    topic <- substitute(topic)
    if (!is.character(topic))
      topic <- deparse(topic)[1L]
  }
  pkgpaths <- find.package(package, lib.loc, verbose = verbose)
  file <- index.search(topic, pkgpaths, firstOnly = TRUE)
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
  tools::Rd2ex(.getHelpFile(file), tf, commentDontrun = !run.dontrun,
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
#' Plot a Slopegraph a la Tufte
#' 
#' This function and documentation are taken from the `"newggslopegraph"` function in the `CGPfunctions` R package.
#' There are slight modifications for use in R-Instat.
#' This function creates a "slopegraph" as conceptualised by Edward Tufte. Slopegraphs are minimalist
#' and efficient presentations of your data that can simultaneously convey the relative rankings,
#' the actual numeric values, and the changes and directionality of the data over time.
#' Takes a dataframe as input, with three named columns being used to draw the plot.
#' Makes the required adjustments to the ggplot2 parameters and returns the plot.
#'
#' @param data a dataframe or an object that can be coerced to a dataframe.
#' Basic error checking is performed, to include ensuring that the named columns
#' exist in the dataframe.
#' @param x a column inside the dataframe that will be plotted on the x axis.
#' Traditionally this is some measure of time.  The function accepts a column of class
#' ordered, factor or character.  NOTE if your variable is currently a "date" class
#' you must convert before using the function with \code{as.character(variablename)}.
#' @param y a column inside the dataframe that will be plotted on the y axis.
#' Traditionally this is some measure such as a percentage.  Currently the function
#' accepts a column of type integer or numeric.  The slopegraph will be most effective
#' when the y variables are not too disparate.
#' @param colour a column inside the dataframe that will be used to group and
#' distinguish different y variables.
#' @param data_label an optional column inside the dataframe that will be used
#'   as the label for the data points plotted.  Can be complex strings and
#'   have `NA` values but must be of class `chr`.  By default `y` is
#'   converted to `chr` and used.
#' @param y_text_size Optionally the font size for the Y axis labels to be displayed.
#' y_text_size = 3 is the default must be a numeric.
#' @param line_thickness Optionally the thickness of the plotted lines that
#' connect the data points. LineThickness = 1 is the default must be a numeric.
#' @param data_text_size Optionally the font size of the plotted data points. DataTextSize = 2.5
#' is the default must be a numeric.
#' @param data_text_colour Optionally the font color of the plotted data points. `"black"`
#' is the default can be either `colors()` or hex value e.g. "#FF00FF".
#' @param data_label_padding Optionally the amount of space between the plotted
#'   data point numbers and the label "box". By default very small = 0.05 to
#'   avoid overlap. Must be a numeric. Too large a value will risk "hiding"
#'   datapoints.
#' @param data_label_line_size Optionally how wide a line to plot around the data
#'   label box. By default = 0 to have no visible border line around the
#'   label. Must be a numeric.
#' @param data_label_fill_colour Optionally the fill color or background of the
#'   plotted data points. `"white"` is the default can be any of the `colors()`
#'   or hex value e.g. "#FF00FF".
#' @param line_colour Optionally the color of the plotted lines. By default it will use
#' the ggplot2 color palette for coloring by \code{colour}. The user may override
#' with \bold{one} valid color of their choice e.g. "black" (see colors() for choices)
#' \bold{OR}
#' they may provide a vector of colors such as c("gray", "red", "green", "gray", "blue")
#' \bold{OR} a named vector like c("Green" = "gray", "Liberal" = "red", "NDP" = "green",
#' "Others" = "gray", "PC" = "blue"). Any input must be character, and the length
#' of a vector \bold{should} equal the number of levels in \code{colour}. If the
#' user does not provide enough colors they will be recycled.
#' @param reverse_x_axis logical, set this value to \code{TRUE} if you want
#' to reverse the **factor levels** on the x scale.
#' @param remove_missing logical, by default set to \code{TRUE} so that if any \code{y}
#' is missing \bold{all rows} for that \code{colour} are removed. If set to \code{FALSE} then
#' the function will try to remove and graph what data it does have. \bold{N.B.} missing values
#' for \code{x} and \code{colour} are never permitted and will generate a fatal error with
#' a warning.
#'
#'
#' @return a plot of type ggplot to the default plot device
#' @export
#'
#' @author Chuck Powell
#' @references Based on: Edward Tufte, Beautiful Evidence (2006), pages 174-176.
#' This function and documentation are taken from the `"newggslopegraph"` function in the `CGPfunctions` R package.
#' Full credit on this function goes to the authors of the `CGPfunctions` R package.
#' @examples
#' 
#' data <- data.frame(
#'           Year = rep(c("2020", "2021"), each = 3),
#'           Value = c(10, 20, 15, 12, 25, 18),
#'           Group = rep(c("A", "B", "C"), times = 2)
#'           )
#' # Use the slopegraph function
#' slopegraph(data, x = Year, y = Value, colour = Group)
slopegraph <- function(data, x, y, colour, data_label = NULL, 
                       y_text_size = 3, 
                       line_thickness = 1, line_colour = "ByGroup", 
                       data_text_size = 2.5, data_text_colour = "black", data_label_padding = 0.05, 
                       data_label_line_size = 0, data_label_fill_colour = "white", 
                       reverse_x_axis = FALSE, 
                       remove_missing = TRUE){
  
  
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed requires a dataframe, plus at least three variables")
  }
  argList <- as.list(match.call()[-1])
  if (!methods::hasArg(data)) {
    stop("You didn't specify a dataframe to use", call. = FALSE)
  }
  Nx <- deparse(substitute(x))
  Ny <- deparse(substitute(y))
  Ncolour <- deparse(substitute(colour))
  if (is.null(argList$data_label)) {
    Ndata_label <- deparse(substitute(y))
    data_label <- argList$y
  } else {
    Ndata_label <- deparse(substitute(data_label))
  }
  Ndata <- argList$data
  if (!methods::is(data, "data.frame")) stop(paste0("'", Ndata, "' does not appear to be a data frame"))
  if (!Nx %in% names(data)) {
    stop(paste0("'", Nx, "' is not the name of a variable in the dataframe"), 
         call. = FALSE)
  }
  if (anyNA(data[[Nx]])) {
    stop(paste0("'", Nx, "' can not have missing data please remove those rows"), 
         call. = FALSE)
  }
  if (!Ny %in% names(data)) {
    stop(paste0("'", Ny, "' is not the name of a variable in the dataframe"), 
         call. = FALSE)
  }
  if (!Ncolour %in% names(data)) {
    stop(paste0("'", Ncolour, "' is not the name of a variable in the dataframe"), 
         call. = FALSE)
  }
  if (!Ndata_label %in% names(data)) {
    stop(paste0("'", Ndata_label, "' is not the name of a variable in the dataframe"), 
         call. = FALSE)
  }
  if (anyNA(data[[Ncolour]])) {
    stop(paste0("'", Ncolour, "' can not have missing data please remove those rows"), 
         call. = FALSE)
  }
  if (!class(data[[Ny]]) %in% c("integer", "numeric")) {
    stop(paste0("Variable '", 
                Ny, "' needs to be numeric"), call. = FALSE)
  }
  if (!"ordered" %in% class(data[[Nx]])) {
    if (!"character" %in% class(data[[Nx]])) {
      if ("factor" %in% class(data[[Nx]])) {
        message(paste0("\nConverting '", Nx, 
                       "' to an ordered factor\n"))
        data[[Nx]] <- factor(data[[Nx]], 
                             ordered = TRUE)
      } else {
        stop(paste0("Variable '", Nx, "' needs to be of class character, factor or ordered"), call. = FALSE)
      }
    }
  }
  data_label <- rlang::enquo(data_label)
  if (reverse_x_axis) {
    data[[Nx]] <- forcats::fct_rev(data[[Nx]])
  }
  NumbOfLevels <- nlevels(factor(data[[Nx]]))
  if (length(line_colour) > 1) {
    if (length(line_colour) < length(unique(data[[Ncolour]]))) {
      message(paste0("\nGiven ", length(line_colour), " colours. Recycling colours because there are ", length(unique(data[[Ncolour]])), " ", Ncolour, "s\n"))
      line_colour <- rep(line_colour, length.out = length(unique(data[[Ncolour]])))
    }
    LineGeom <- list(ggplot2::geom_line(ggplot2::aes(colour = {{colour}}), linewidth = line_thickness), 
                     ggplot2::scale_colour_manual(values = line_colour))
  }
  else {
    if (line_colour == "ByGroup") {
      LineGeom <- list(ggplot2::geom_line(ggplot2::aes(colour = {{colour}}, 
                                                       alpha = 1), linewidth = line_thickness))
    }
    else {
      LineGeom <- list(ggplot2::geom_line(ggplot2::aes_(), linewidth = line_thickness, 
                                          colour = line_colour))
    }
  }
  if (anyNA(data[[Ny]])) {
    if (remove_missing) data <- data %>% dplyr::group_by({{colour}}) %>% dplyr::filter(!anyNA({{y}})) %>% droplevels()
    else data <- data %>% dplyr::filter(!is.na({{y}}))
  }
  data %>% ggplot2::ggplot(ggplot2::aes(group = {{colour}}, y = {{y}}, x = {{x}})) +
    LineGeom +
    # note: this may conflict with other label in R, in which case we need to rewrite this
    ggrepel::geom_text_repel(data = . %>% dplyr::filter({{x}} == min({{x}})),
                             ggplot2::aes(label = {{colour}}), hjust = "left",
                             box.padding = 0.1, point.padding = 0.1, 
                             segment.colour = "gray", segment.alpha = 0.6, 
                             fontface = "bold", size = y_text_size,
                             nudge_x = -1.95, direction = "y", force = 0.5,
                             max.iter = 3000) +
    ggrepel::geom_text_repel(data = . %>% dplyr::filter({{x}} == max({{x}})), 
                             ggplot2::aes(label = {{colour}}), hjust = "right", 
                             box.padding = 0.1, point.padding = 0.1, 
                             segment.colour = "gray", segment.alpha = 0.6,
                             fontface = "bold", size = y_text_size,
                             nudge_x = 1.95, direction = "y", force = 0.5,
                             max.iter = 3000) +
    ggplot2::geom_label(ggplot2::aes(label = !!rlang::sym(Ndata_label)), 
                        size = data_text_size, 
                        label.padding = ggplot2::unit(data_label_padding, "lines"),
                        label.size = data_label_line_size, 
                        colour = data_text_colour, 
                        fill = data_label_fill_colour)
}

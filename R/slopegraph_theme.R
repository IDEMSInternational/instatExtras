#' Slope graph theme
#'
#' @description  A function that generates a theme for slopegraph plots.
#'
#' @param x_text_size The font size for the x-axis text (default: 12).
#'
#' @return A list of theme elements for slopegraph plots.
#'
#' @export
#'
#' @examples
#' # Example 1: Generate a slopegraph theme with default parameters
#' #theme <- slopegraph_theme()
#'
#' # Example 2: Generate a slopegraph theme with custom x-axis text size
#' #theme <- slopegraph_theme(x_text_size = 14)
slopegraph_theme <- function(x_text_size = 12){
  list(ggplot2::scale_x_discrete(position = "top"), 
       ggplot2::theme(legend.position = "none"),
       ggplot2::theme(axis.text.y = ggplot2::element_blank()),
       ggplot2::theme(panel.border = ggplot2::element_blank()), 
       ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()),
       ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank()), 
       ggplot2::theme(axis.title.x = ggplot2::element_blank()),
       ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()), 
       ggplot2::theme(axis.text.x.top = ggplot2::element_text(size = x_text_size, face = "bold")),
       ggplot2::theme(axis.ticks = ggplot2::element_blank()))
}

#' Visualise a ranking network using `ggplot2` and `ggraph`
#'
#' @param object An object of class `rankings` or `grouped_rankings`.
#' @param fluctuate_widths Logical; if `TRUE`, edge widths reflect edge weights (e.g., strength or frequency of preference).
#' @param ... Additional arguments passed to `ggraph::ggraph` functions.
#'
#' @return A `ggplot2` network plot representing the ranking structure.
#' @export
#' 
#' @examples
#' library("PlackettLuce")
#' 
#' R = matrix(c(1, 2, 0, 0,
#'              4, 1, 2, 3,
#'              2, 4, 1, 3,
#'              1, 2, 3, 0,
#'              2, 1, 3, 0,
#'              1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' R = as.rankings(R)
#' 
#' plot_network(R, fluctuate_widths = TRUE)
plot_network <- function(object, fluctuate_widths = FALSE, ...) {
  
  if (!requireNamespace("PlackettLuce", quietly = TRUE)) {
    stop("Package 'PlackettLuce' is required.")
  }
  if (!requireNamespace("ggraph", quietly = TRUE) ||
      !requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Packages 'ggraph' and 'tidygraph' are required.")
  }
  
  # Convert grouped rankings to regular rankings
  if (inherits(object, "grouped_rankings")) {
    object <- as.rankings(object)
  }
  
  R <- object
  
  # Ensure column names are present
  if (is.null(colnames(R))) {
    colnames(R) <- paste0("Item", seq_len(ncol(R)))
  }
  
  # Create PlackettLuce::adjacency matrix and convert to graph
  adj <- PlackettLuce::adjacency(R)
  dimnames(adj) <- list(colnames(R), colnames(R))
  
  # Use a helper to convert to graph (assumes btdata is defined)
  if (!exists("btdata", mode = "function")) {
    stop("The function 'btdata' must be available in the environment.")
  }
  adj_graph <- btdata(adj, return_graph = TRUE)$graph
  net_tbl <- tidygraph::as_tbl_graph(adj_graph)
  
  # Build plot
  graph_to_return <- ggraph::ggraph(net_tbl, layout = "fr") + 
    ggraph::geom_node_point(size = 5, colour = "steelblue") +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, vjust = 1.5) +
    ggplot2::theme_void() +
    ggplot2::ggtitle("Ranking Network") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  # Add edges
  if (!fluctuate_widths) {
    graph_to_return <- graph_to_return +
      ggraph::geom_edge_link(
        arrow = grid::arrow(length = grid::unit(4, "mm"), type = "closed"), 
        end_cap = ggraph::circle(3, 'mm')
      )
  } else {
    graph_to_return <- graph_to_return +
      ggraph::geom_edge_link(
        ggplot2::aes(width = weight), 
        arrow = grid::arrow(length = grid::unit(4, "mm"), type = "closed"), 
        end_cap = ggraph::circle(3, 'mm'),
        alpha = 0.8, 
        colour = "grey"
      )
  }
  
  return(graph_to_return)
}


# function from https://github.com/EllaKaye/BradleyTerryScalable
# which unfortunately was removed from CRAN
btdata = function(x, return_graph = FALSE) {
  
  # if x is a table, convert it to a matrix
  if (is.table(x)) {
    attr(x, "class") = NULL
    attr(x, "call") = NULL
  }
  
  if ((methods::is(x, "Matrix") | is.matrix(x) )) {
    # check dimensions/content
    if (dim(x)[1] != dim(x)[2]) stop("If x is a matrix or table, it must be a square")
    if(is.matrix(x)) {if (!is.numeric(x)) stop("If x is a matrix or table, all elements must be numeric")}
    if(methods::is(x, "Matrix")) {if (!is.numeric(as.vector(x))) stop("If x is a matrix or table, all elements must be numeric")}
    if (any(x < 0)) stop("If x is a matrix or table, all elements must be non-negative")
    if(!identical(rownames(x), colnames(x))) stop("If x is a matrix or table, rownames and colnames of x should be the same")
    if (anyDuplicated(rownames(x)) > 0) {
      
      arg = deparse(substitute(x))
      stop("If x is a matrix or table with row- and column names, these must be unique. Consider fixing with rownames(", arg, ") = colnames(", arg, ") = make.names(rownames(", arg, "), unique = TRUE)")
    }
    
    # ensure wins is a dgCMatrix
    if (is.matrix(x)) wins = Matrix::Matrix(x, sparse = TRUE)
    else wins = x
    if (class(wins) != "dgCMatrix") wins = methods::as(wins, "dgCMatrix")
    g = igraph::graph_from_adjacency_matrix(wins, weighted = TRUE, diag = FALSE)
  }
  
  else stop("x must be a 3 or 4 column dataframe, a directed igraph object, or square matrix or contingency table.")
  
  
  ## get components
  comp = igraph::components(g, mode = "strong")
  components = igraph::groups(comp)
  
  # name the rows and columns of the wins matrix, if NULL
  if (is.null(unlist(dimnames(wins)))) {
    K = nrow(wins)
    dimnames(wins) = list(1:K, 1:K)
  }
  
  # return
  result = list(wins = wins, components = components)
  if (return_graph) result$graph = g
  class(result) = c("btdata", "list")
  result
}
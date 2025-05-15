#' Plot a Plackett-Luce Tree with Node Worth Parameters
#'
#' This function takes a `PlackettLuce` model tree (fitted using `pltree()` from the `PlackettLuce` package)
#' and visualises the structure of the tree alongside dotplots showing the worth parameters and confidence intervals
#' at each terminal node.
#'
#' @param tree A fitted Plackett-Luce model tree, typically an object of class `party` or `pltree`.
#'
#' @return A combined `ggplot` object containing the tree structure (split variable and level labels) above,
#' and dotplots of worth parameters at terminal nodes below. Each dotplot shows point estimates and confidence
#' intervals for each item within the node.
#'
#' @details
#' The function identifies the terminal nodes of the tree and extracts fitted Plackett-Luce models from each.
#' Worth parameters and their 95% confidence intervals are computed for each item.
#' The root node's split variable, p-value, and groupings are used to draw a schematic tree.
#' 
#' The resulting plot combines:
#' \itemize{
#'   \item A tree diagram summarising the root node's split (currently assumes only one split)
#'   \item Dotplots of worth parameters for each terminal node
#' }
#' 
#' @note Requires `ggplot2`, `patchwork`, `partykit`, `purrr`, `tidyr`, and `dplyr`.
#'
#' @examples
#' \dontrun{
#'   tree <- pltree(rankings ~ covariate, data = your_data)
#'   plot_pltree(tree)
#' }
#' 
#' @export
plot_pltree <- function(tree) {
  library(patchwork)
  
  terminal_ids <- partykit::nodeids(tree, terminal = TRUE)
  
  find_node_by_id <- function(node, id) {
    if (partykit::id_node(node) == id) return(node)
    if (partykit::is.terminal(node)) return(NULL)
    for (kid in partykit::kids_node(node)) {
      result <- find_node_by_id(kid, id)
      if (!is.null(result)) return(result)
    }
    return(NULL)
  }
  
  worth_data <- purrr::map_dfr(terminal_ids, function(nid) {
    node <- find_node_by_id(partykit::node_party(tree), nid)
    model <- partykit::info_node(node)$object
    worth <- coef(model)
    se <- sqrt(diag(vcov(model)))
    n <- length(model$rankings)
    tidyr::tibble(
      node = paste0("Node ", nid),
      item = names(worth),
      worth = as.numeric(worth),
      lower = worth - 1.96 * se,
      upper = worth + 1.96 * se,
      n = n
    )
  })
  
  worth_data <- worth_data %>%
    dplyr::mutate(item = factor(item, levels = rev(unique(item))),
                  item_label = paste0(item, " (", round(worth, 2), ")"),
                  node_label = paste0(node, " (n = ", n, ")"))
  
  # Get root node info
  root_node <- partykit::node_party(tree)
  split_var_index <- partykit::split_node(root_node)$varid
  split_var_name <- names(tree$data)[split_var_index]
  split_levels <- levels(tree$data[[split_var_name]])
  split_index <- partykit::split_node(root_node)$index
  
  left_levels <- split_levels[split_index == 1]
  right_levels <- split_levels[split_index == 2]
  
  left_label <- paste(left_levels, collapse = ", ")
  right_label <- paste(right_levels, collapse = ", ")
  
  p_val <- partykit::info_node(root_node)$p.value
  p_val_label <- formatC(p_val, format = "e", digits = 2)
  root_label <- paste0(split_var_name, "\np = ", p_val_label)
  
  left_label <- paste(left_levels, collapse = ", ")
  right_label <- paste(right_levels, collapse = ", ")
  edges <- tidyr::tibble(
    x = c(1, 1),
    xend = c(0.5, 1.5),
    y = c(2, 2),
    yend = c(1, 1),
    label = c(left_label, right_label),
    label_x = c(0.75, 1.25),
    label_y = c(1.5, 1.5)
  )
  
  # Plot A: Tree structure only
  tree_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          linewidth = 0.8, color = "gray40") +
    # Replace Root text with a labelled box
    ggplot2::geom_label(ggplot2::aes(x = 1, y = 2.15, label = root_label),
                        size = 3.5, label.size = 0.3, fill = "white", inherit.aes = FALSE) + 
    
    ggplot2::geom_text(ggplot2::aes(x = 0.75, y = 1.5, label = left_label), size = 3.5, inherit.aes = FALSE) +
    ggplot2::geom_text(ggplot2::aes(x = 1.25, y = 1.5, label = right_label), size = 3.5, inherit.aes = FALSE) +
    ggplot2::theme_void() +
    ggplot2::xlim(0, 2) + ggplot2::ylim(0.8, 2.4)
  
  # Plot B: Worth plots in facets
  dotplot <- ggplot2::ggplot(worth_data, ggplot2::aes(x = worth, y = item)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.2, color = "blue") +
    ggplot2::geom_text(ggplot2::aes(label = round(worth, 2)), hjust = -0.3, size = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::facet_wrap(~ node_label, nrow = 1) +
    ggplot2::scale_y_discrete() +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_text(size = 8)) +
    ggplot2::labs(x = "Worth Parameter")
  
  # Combine plots
  final_plot <- tree_plot / dotplot + plot_layout(heights = c(1, 3))
  
  return(final_plot)
}

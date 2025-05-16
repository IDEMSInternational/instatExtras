#' Plot a Plackett-Luce Tree using ggplot2
#'
#' Recursively plots a tree structure and terminal node worth dotplots using only ggplot2.
#' Each terminal node's worths are displayed in a separate facet below the tree.
#'
#' @param tree A fitted PlackettLuce model tree (class "party").
#'
#' @return A ggplot2 object showing the tree and corresponding worth dotplots.
#' @export
plot_pltree <- function(tree) {
  # Recursive tree layout ----------------------------------------------------
  layout_tree <- function(node, x = 1, y = 0, depth = 0, counter = 1) {
    this_id <- partykit::id_node(node)
    is_terminal <- partykit::is.terminal(node)
    label <- if (!is_terminal) {
      split <- partykit::split_node(node)
      var_name <- names(attr(tree, "data"))[split$varid]
      paste0(var_name, "\np = ",
             formatC(partykit::info_node(node)$p.value,
                     format = "e",
                     digits = 2))
    } else {
      paste("Node", this_id)
    }
    
    layout <- tidyr::tibble(id = this_id,
                            label = label,
                            x = x,
                            y = -depth,
                            terminal = is_terminal)
    
    if (is_terminal) return(layout)
    
    kids <- partykit::kids_node(node)
    n_kids <- length(kids)
    x_positions <- seq(x - 0.5, x + 0.5, length.out = n_kids)
    
    child_layouts <- purrr::map2_dfr(kids, x_positions,
                                     ~ layout_tree(.x, x = .y, y = y - 1, depth = depth + 1))
    
    dplyr::bind_rows(layout, child_layouts)
  }
  
  get_edges <- function(layout) {
    edges <- layout %>%
      dplyr::inner_join(layout, by = character(), suffix = c("_parent", "_child")) %>%
      dplyr::filter(y_child == y_parent - 1,
                    abs(x_child - x_parent) <= 0.51)
    return(edges)
  }
  
  root_node <- partykit::node_party(tree)
  layout <- layout_tree(root_node)
  
  edges <- get_edges(layout)
  
  # Worth data ---------------------------------------------------------------
  terminal_ids <- layout %>%
    dplyr::filter(terminal) %>%
    dplyr::pull(id)
  
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
    node <- find_node_by_id(root_node, nid)
    model <- partykit::info_node(node)$object
    worth <- as.numeric(coef(model))
    se <- sqrt(diag(vcov(model)))
    n <- length(model$rankings)
    tidyr::tibble(
      node = paste0("Node ", nid),
      item = names(coef(model)),
      worth = worth,
      lower = worth - 1.96 * se,
      upper = worth + 1.96 * se,
      n = n
    )
  }) %>%
    dplyr::mutate(item = factor(item,levels = rev(unique(item))),
                  node_label = paste0(node, " (n = ", n, ")"))
  
  # Tree plot ----------------------------------------------------------------
  tree_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges,
                          ggplot2::aes(x = x_parent, y = y_parent, xend = x_child, yend = y_child),
                          color = "grey40") +
    ggplot2::geom_label(data = layout,
                        ggplot2::aes(x = x, y = y, label = label),
                        fill = "white", size = 3.5, label.size = 0.3) +
    ggplot2::theme_void()
  
  # Dotplot ------------------------------------------------------------------
  dotplot <- ggplot2::ggplot(worth_data, ggplot2::aes(x = worth, y = item)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper), height = 0.2, color = "blue") +
    ggplot2::geom_text(ggplot2::aes(label = round(worth, 2)), hjust = -0.3, size = 3) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::facet_wrap(~ node_label, nrow = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_text(size = 8)) +
    ggplot2::labs(x = "Worth Parameter")
  
  # Combine ------------------------------------------------------------------
  tree_plot / dotplot + patchwork::plot_layout(heights = c(1, 3))
}

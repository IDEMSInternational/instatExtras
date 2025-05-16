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
  tree_data <- model.frame(tree)
  
  layout_tree <- function(node, x = 1, y = 0, depth = 0) {
    this_id <- partykit::id_node(node)
    is_terminal <- partykit::is.terminal(node)
    label <- if (!is_terminal) {
      split <- partykit::split_node(node)
      var_name <- names(tree_data)[split$varid]
      paste0(var_name, "\np = ",
             formatC(partykit::info_node(node)$p.value,
                     format = "e",
                     digits = 2))
    } else {
      paste("Node", this_id)
    }
    
    node_layout <- tidyr::tibble(id = this_id,
                                 label = label,
                                 x = x,
                                 y = -depth,
                                 terminal = is_terminal)
    edge_labels <- tidyr::tibble()
    
    if (is_terminal) return(list(layout = node_layout, edges = edge_labels))
    
    kids <- partykit::kids_node(node)
    split <- partykit::split_node(node)
    split_var <- names(tree_data)[split$varid]
    split_levels <- levels(tree_data[[split_var]])
    split_index <- split$index
    n_kids <- length(kids)
    x_positions <- seq(x - 0.5, x + 0.5, length.out = n_kids)
    
    results <- purrr::map2(kids, seq_len(n_kids), function(kid, i) {
      kid_result <- layout_tree(kid, x = x_positions[i], y = y - 1, depth = depth + 1)
      levels_this_kid <- split_levels[split_index == i]
      edge_label <- paste(levels_this_kid, collapse = ", ")
      edge <- dplyr::tibble(
        parent = this_id,
        child = partykit::id_node(kid),
        x = x,
        y = -depth,
        xend = x_positions[i],
        yend = -(depth + 1),
        label = edge_label,
        label_x = (x + x_positions[i]) / 2,
        label_y = -(depth + 0.5)
      )
      list(layout = kid_result$layout, edges = dplyr::bind_rows(kid_result$edges, edge))
    })
    
    child_layouts <- dplyr::bind_rows(purrr::map(results, "layout"))
    all_edges <- dplyr::bind_rows(purrr::map(results, "edges"))
    
    list(layout = dplyr::bind_rows(node_layout, child_layouts), edges = all_edges)
  }
  
  root_node <- partykit::node_party(tree)
  layout_result <- layout_tree(root_node)
  layout <- layout_result$layout
  edges <- layout_result$edges
  
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
    tibble::tibble(
      node = paste0("Node ", nid),
      item = names(coef(model)),
      worth = worth,
      lower = worth - 1.96 * se,
      upper = worth + 1.96 * se,
      n = n
    )
  }) %>%
    dplyr::mutate(item = factor(item, levels = rev(unique(item))),
                  node_label = paste0(node, " (n = ", n, ")"))
  
  tree_plot <- ggplot2::ggplot()
  if (nrow(edges) > 0){
    tree_plot <- tree_plot +
      ggplot2::geom_segment(data = edges,
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            color = "grey40") +
      ggplot2::geom_text(data = edges,
                         ggplot2::aes(x = label_x, y = label_y, label = label),
                         size = 3.2)
  }
  tree_plot <- tree_plot +
    ggplot2::geom_label(data = layout,
                        ggplot2::aes(x = x, y = y, label = label),
                        fill = "white", size = 3.5, label.size = 0.3) +
    ggplot2::theme_void()
  
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
  
  tree_plot / dotplot + patchwork::plot_layout(heights = c(1, 3))
}
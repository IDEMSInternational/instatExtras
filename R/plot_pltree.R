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
plot_pltree <- function (tree){
  library(PlackettLuce)
  library(partykit)
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(patchwork)
  terminal_ids <- nodeids(tree, terminal = TRUE)
  find_node_by_id <- function(node, id) {
    if (id_node(node) == id) 
      return(node)
    if (is.terminal(node)) 
      return(NULL)
    for (kid in kids_node(node)) {
      result <- find_node_by_id(kid, id)
      if (!is.null(result)) 
        return(result)
    }
    return(NULL)
  }
  worth_data <- map_dfr(terminal_ids, function(nid) {
    node <- find_node_by_id(node_party(tree), nid)
    model <- info_node(node)$object
    worth <- coef(model)
    se <- sqrt(diag(vcov(model)))
    n <- length(model$rankings)
    tibble(node = paste0("Node ", nid), item = names(worth), 
           worth = as.numeric(worth), lower = worth - 1.96 * 
             se, upper = worth + 1.96 * se, n = n)
  })
  worth_data <- worth_data %>% mutate(item = factor(item, levels = rev(unique(item))), 
                                      item_label = paste0(item, " (", round(worth, 2), ")"), 
                                      node_label = paste0(node, " (n = ", n, ")"))
  layout_data <- tibble(node = c("Root"), x = c(1), y = c(2))
  edges <- tibble(x = c(1, 1), xend = c(0.5, 1.5), y = c(2, 
                                                         2), yend = c(1, 1), label = c("Less Than 2x / Month or Rarely", 
                                                                                       "Every Week"), label_x = c(0.75, 1.25), label_y = c(1.5, 
                                                                                                                                           1.5))
  root_node <- node_party(tree)
  split_var_index <- split_node(root_node)$varid
  split_var_name <- names(tree$data)[split_var_index]
  p_val <- info_node(root_node)$p.value
  p_val_label <- formatC(p_val, format = "e", digits = 2)
  root_label <- paste0(split_var_name, "\np = ", p_val_label)
  tree_plot <- ggplot() + geom_segment(data = edges, aes(x = x, 
                                                         y = y, xend = xend, yend = yend), linewidth = 0.8, color = "gray40") + 
    geom_label(aes(x = 1, y = 2.15, label = root_label), 
               size = 3.5, label.size = 0.3, fill = "white", inherit.aes = FALSE) + 
    geom_text(data = edges, aes(x = label_x, y = label_y, 
                                label = label), size = 3.5) + theme_void() + xlim(0, 
                                                                                  2) + ylim(0.8, 2.4)
  dotplot <- ggplot(worth_data, aes(x = worth, y = item)) + 
    geom_point(size = 2) + geom_errorbarh(aes(xmin = lower, 
                                              xmax = upper), height = 0.2, color = "blue") + geom_text(aes(label = round(worth, 
                                                                                                                         2)), hjust = -0.3, size = 3) + geom_vline(xintercept = 0, 
                                                                                                                                                                   linetype = "dashed", color = "grey60") + facet_wrap(~node_label, 
                                                                                                                                                                                                                       nrow = 1) + scale_y_discrete() + theme_minimal() + theme(strip.text = element_text(face = "bold"), 
                                                                                                                                                                                                                                                                                axis.title.y = element_blank(), axis.title.x = element_text(size = 10), 
                                                                                                                                                                                                                                                                                axis.text.y = element_text(size = 8)) + labs(x = "Worth Parameter")
  final_plot <- tree_plot/dotplot + plot_layout(heights = c(1, 
                                                            3))
  return(final_plot)
}
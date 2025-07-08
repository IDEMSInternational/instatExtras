#' Check and report on strong connectivity of a Plackett–Luce network
#'
#' Runs \code{PlackettLuce::connectivity()} on a rankings object,
#' captures any warning messages, and prints a concise,
#' human-readable report of cluster structure.
#'
#' @param X A rankings object, as produced by \code{as.rankings()}.
#'   Default is the first element of \code{rankings_object}.
#' @return Invisibly returns the result of
#'   \code{PlackettLuce::connectivity(X)}, while printing either:
#'   \itemize{
#'     \item A “strongly connected” message if there is a single
#'           cluster, or
#'     \item A “not strongly connected” report showing
#'           the number of clusters, their sizes, and the
#'           membership vector otherwise.
#'   }
#' @examples
#' # Strongly connected example
#' mat <- matrix(c(
#'   1,2,3,4,
#'   2,3,4,1,
#'   3,4,1,2,
#'   4,1,2,3
#' ), ncol = 4, byrow = TRUE)
#' colnames(mat) <- letters[1:4]
#' X <- as.rankings(mat)
#' connectivity_check(X)
#'
#' # Disconnected example
#' mat2 <- matrix(c(
#'   1,0,0,0,
#'   0,2,0,0,
#'   0,0,3,4,
#'   0,0,4,3
#' ), ncol = 4, byrow = TRUE)
#' colnames(mat2) <- letters[1:4]
#' X2 <- as.rankings(mat2)
#' connectivity_check(X2)
#' @export
connectivity_check <- function(X = rankings_object[[1]]) {
  # 1. Capture any messages produced by connectivity()
  msgs <- capture.output({
    res <- PlackettLuce::connectivity(X)
  }, type = "message")
  
  # 2. Print report
  if (length(msgs) > 0) {
    # Disconnected
    cat("WARNING: Network is not strongly connected.\n\n")
    cat(sprintf("  • Clusters found: %d  (need 1 for strong connectivity)\n", res$no))
    cat(sprintf("  • Cluster sizes: %s\n\n", paste(res$csize, collapse = ", ")))
    cat("  Cluster membership:\n")
    for (i in seq_len(res$no)) {
      members <- names(res$membership)[res$membership == i]
      cat(sprintf("    – Cluster %d (%d items): %s\n",
                  i, length(members), paste(members, collapse = ", ")))
    }
  } else {
    # Strongly connected
    cat("Network is strongly connected.\n")
    cat("There is 1 cluster. All varieties are connected in the same cluster.\n")
  }
  
  invisible(res)
}
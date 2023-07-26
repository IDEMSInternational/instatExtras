#' Threshold Plot
#'
#' @description  This function generates threshold plots for a given dataset.
#'
#' @param x Numeric vector of data values.
#' @param r Numeric vector of quantiles defining the range of thresholds. Default is to use the 75th and 99th percentiles of x.
#' @param type Character string specifying the type of extreme value distribution to fit. Possible values are "GP" , "PP" , or "Exponential". Default is "GP".
#' @param nint Integer specifying the number of intervals between the lower and upper thresholds. Default is 10.
#' @param alpha Numeric value specifying the significance level for confidence intervals. Default is 0.05.
#' @param na.action Function to handle missing values. Default is na.omit.
#' @param xlb Character string specifying the x-axis label for the plot. Default is an empty string.
#' @param main Character string specifying the main title for the plot. Default is NULL.
#' @param verbose Logical value indicating whether to print additional output. Default is FALSE.
#' @param ... Additional arguments to be passed to the fitting and confidence interval functions.
#'
#' @return A list of threshold plots, including location, scale, and shape, depending on the specified type.
#' @export
#'
#' @examples
#' # Generate threshold plots for a dataset
#' data <- c(2.1, 2.2, 3.4, 4.5, 5.6, 6.7, 7.8, 8.9, 9.0, 10.1)
#' threshold_Plot(data, type = "GP")
threshold_Plot <- function(x, r, type = c("GP", "PP", "Exponential"), nint = 10,
                           alpha = 0.05, na.action = stats::na.omit, xlb = "", main = NULL , verbose = FALSE,
                           ...) {
  type <- match.arg(type)
  x <- na.action(x)
  n <- length(x)
  lst_plots <- list()
  if (missing(r)) {
    r <- quantile(x, probs = c(0.75, 0.99))
  }
  u.i <- matrix(seq(from = r[1], to = r[2],length.out =  nint), ncol = 1)
  thfun <- function(u, x, type, a, verbose, ...) {
    fit <- try(extRemes::fevd(
      x = x, threshold = u, type = type, verbose = verbose,
      ...
    ), silent = verbose)
    if (verbose) {
      print(fit)
    }
    if (all(class(fit) != "try-error")) {
      if (!is.element(type, c("PP", "Exponential"))) {
        res <- try(distillery::ci(fit,
                                  type = "parameter", alpha = a,
                                  R = 100, tscale = TRUE, ...
        ), silent = verbose)
      } else {
        res <- try(distillery::ci(fit,
                                  type = "parameter", alpha = a,
                                  R = 100, ...
        ), silent = verbose)
      }
      if (verbose) {
        print(res)
      }
    }
    else {
      res <- fit
    }
    if (any(class(res) == "try-error")) {
      if (type == "PP") {
        res <- matrix(NA, 3, 3)
      } else if (type != "Exponential") {
        res <- matrix(NA, 2, 3)
      } else {
        res <- rep(NA, 3)
      }
    }
    return(res)
  }
  out <- apply(u.i, 1, thfun,
               x = x, type = type, a = alpha,
               verbose = verbose, ...
  )
  if (type == "PP") {
    rownames(out) <- c(
      "low.loc", "low.scale", "low.shape",
      "location", "scale", "shape", "up.loc", "up.scale",
      "up.shape"
    )
  } else if (type != "Exponential") {
    rownames(out) <- c(
      "low.t.scale", "low.shape", "t.scale",
      "shape", "up.t.scale", "up.shape"
    )
  } else {
    rownames(out) <- c("low.scale", "scale", "up.scale")
  }
  m1 <- deparse(match.call())
  if (type == "PP") {
    yl <- range(c(out[c("low.loc", "location", "up.loc"), ]), finite = TRUE)
    lst_plots[[1]] <- ggplot2::qplot(
      x = u.i, y = out["location", ], ylim = yl, xlab = xlb,
      ylab = "location", geom = c("line", "point"), main = main
    ) +
      ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = out["low.loc", ], ymax = out["up.loc", ]))
    yl <- range(c(out[c("low.scale", "scale", "up.scale"), ]), finite = TRUE)
    lst_plots[[2]] <- ggplot2::qplot(
      x = u.i, y = out["scale", ], ylim = yl, xlab = xlb, ylab = "scale",
      geom = c("point", "line")
    ) +
      ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = out["low.scale", ], ymax = out["up.scale", ]))
    yl <- range(c(out[c("low.shape", "shape", "up.shape"), ]), finite = TRUE)
    lst_plots[[3]] <- ggplot2::qplot(u.i, out["shape", ],
                                     ylim = yl, xlab = "Threshold",
                                     ylab = "shape", geom = c("point", "line")
    ) +
      ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = out["low.shape", ], ymax = out["up.shape", ]))
  }
  else if (type != "Exponential") {
    yl <- range(c(out[c("low.t.scale", "t.scale", "up.t.scale"), ]), finite = TRUE)
    lst_plots[[1]] <- ggplot2::qplot(
      x = u.i, y = out["t.scale", ], ylim = yl, xlab = xlb, ylab = "reparameterized scale",
      geom = c("point", "line"), main = main) +
      ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = out["low.t.scale", ], ymax = out["up.t.scale", ]))
    yl <- range(c(out[c("low.shape", "shape", "up.shape"), ]), finite = TRUE)
    lst_plots[[2]] <- ggplot2::qplot(
      x = u.i, y = out["shape", ], ylim = yl, xlab = "Threshold",
      ylab = "shape", geom = c("point", "line")
    ) +
      ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = out["low.shape", ], ymax = out["up.shape", ]))
  }
  else {
    yl <- range(c(out[c("low.scale", "scale", "up.scale"), 
    ]), finite = TRUE)
    return(ggplot2::qplot(
      x = u.i, y = out["scale", ], ylim = yl, xlab = "Threshold",
      ylab = "scale", geom = c("point", "line"), main = main
    ) +
      ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = out["low.scale", ], ymax = out["up.scale", ])))
  }
  patchwork::wrap_plots(lst_plots, ncol = 1) 
}

# This function produces multiple threshold plots for various stations at a time.
plot_multiple_threshold <- function(data, station_col_name, element_col_name, r, type = c("GP", "PP", "Exponential"), nint = 10,
                                    alpha = 0.05, ncol = 1, xlb = "", main = NULL , verbose = FALSE,...) {
  if (!missing(station_col_name)) {
    plts <- list()
    station_col <- data[, station_col_name]
    stations <- unique(station_col)
    for (i in seq_along(stations)) {
      d <- data[station_col == stations[i], ]
      element_col <- d[, element_col_name]
      plts[[i]] <- threshold_Plot(x = element_col, main = stations[i], r = r, type = type, nint = nint, alpha = alpha, verbose = verbose)
    }
    patchwork::wrap_plots(plts, ncol = ncol)
  }
  else {
    element_col <- data[, element_col_name]
    threshold_Plot(x = element_col, r = r, type = type, nint = nint, alpha = alpha, verbose = verbose)
  }
}


plot_declustered <- function(data, station_col_name, element_col_name, threshold, r = NULL, xlab = NULL, ylab = NULL, ncol = 1, print_summary = FALSE) {
  if (!missing(station_col_name)) {
    plts <- list()
    station_col <- data[, station_col_name]
    stations <- unique(station_col)
    for (i in seq_along(stations)) {
      station <- stations[i]
      d <- data[station_col == station, ]
      obj <- texmex::declust(y = na.exclude(d[, element_col_name]), r = r, threshold = threshold)
      if (print_summary) {
        cat("Station:", paste0("", station, ""), "\n \n")
        cat("Threshold", obj$threshold, "\n")
        cat("Declustering using the intervals method, run length", obj$r, "\n")
        cat("Identified", obj$nClusters, "clusters.", "\n")
        cat("------------------------------------------------------", "\n \n")
      } else {
        plts[[i]] <- obj %>%
          ggplot2::ggplot(xlab = xlab, ylab = ylab, main = stations[i])
      }
    }
    if (!print_summary) {
      patchwork::wrap_plots(plts, ncol = ncol)
    }
  }
  else {
    obj <- texmex::declust(y = na.exclude(data[, element_col_name]), r = r, threshold = threshold)
    if (print_summary) {
      cat("Threshold", obj$threshold, "\n")
      cat("Declustering using the intervals method, run length", obj$r, "\n")
      cat("Identified", obj$nClusters, "clusters.", "\n")
    } else {
      obj %>% ggplot2::ggplot(xlab = xlab, ylab = ylab)
    }
  }
}
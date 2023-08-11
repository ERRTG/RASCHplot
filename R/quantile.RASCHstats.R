#' Sample Quantiles
#'
#' Produces sample quantiles corresponding to the given probabilities for a \code{RASCHstats} object. The smallest observation corresponds to a probability of 0 and the largest to a probability of 1.
#'
#' @param x A \code{RASCHstats} object.
#' @param probs Vector of percentiles for \code{extreme}.
#'
#' @method quantile RASCHstats
#' @rdname quantile.RASCHstats
#' @export
#'
quantile.RASCHstats <- function(x, probs) {

  if (!inherits(x, "RASCHstats")) {
    stop("use only with \"RASCHstats\" objects")
  }


  if (missing(probs)) {
    probs <- list()
    probs$min <- c(0.025, 0.05)
    probs$max <- c(0.95, 0.975)
  }

  mat <- bystat(x)
  quants <- lapply(mat, function(i) list("min" = quantile(apply(i, 1, min),
                                                          prob = probs$min, na.rm = TRUE),
                                         "max" = quantile(apply(i, 1, max),
                                                          prob = probs$max, na.rm = TRUE)))

  quants
}

#' Grouping of total scores function
#'
#' This function groups total scores by quantiles.
#'
#' @param x A numeric vector which is to be converted to a factor by cutting.
#' @param probs A numeric vector of probabilities with values in [0,1].
#' @param min0 Logical flag for shifting responses such that lowest category is 0. Default is \code{TRUE}.
#'
#' @return Vector
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#' lower.groups <- CUTscores(x = it.SPADI, probs = seq(0, 1, length.out = 6))
#' CICCplot(model = model.SPADI, lower.groups = lower.groups)
#'
#' @export CUTscores
#'
CUTscores <- function(x, probs = seq(0, 1, length.out = 6), min0 = TRUE) {

  if (min0) {
    ri.min <- apply(x, 2, min, na.rm = TRUE)

    #if no 0 responses
    if(any(ri.min > 0)){
      warning(paste0(
        "\n",
        paste("The following items have no 0-responses:"),
        "\n",
        paste(colnames(x)[ri.min > 0], collapse=" "),
        "\n",
        paste("Responses are shifted such that lowest category is 0.")
      ), call. = FALSE, immediate.=TRUE)
    }

    #shift down to 0
    x <- t(apply(x,1,function(y) {y-ri.min}))
  }

  x <- x[which(rowSums(x) != max(x)*ncol(x)),]
  qu <- quantile(rowSums(x, na.rm = TRUE), probs)
  cuts <- CUT(rowSums(x, na.rm = TRUE),
              breaks = qu, include.lowest = TRUE)

  lower.cuts <- cuts$ranges$lower
  lower.groups <- as.numeric(lower.cuts)
  lower.groups
}
#'
#' Convert numeric to intervals
#' @param x a numeric vector which is to be converted to a factor by cutting.
#' @param breaks either a numeric vector of two or more unique cut points or a single number (greater than or equal to 2) giving the number of intervals into which x is to be cut.
#' @param labels labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation. If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param include.lowest logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param dig.lab integer which is used when labels are not given. It determines the number of digits used in formatting the break numbers.
#' @param ordered_result logical: should the result be an ordered factor?
#' @param ... further arguments passed to or from other methods.
#' @noRd
CUT <- function (x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3, ordered_result = FALSE, ...)
{
  if (!is.numeric(x))
    stop("'x' must be numeric")
  if (length(breaks) == 1L) {
    if (is.na(breaks) || breaks < 2L)
      stop("invalid number of intervals")
    nb <- as.integer(breaks + 1)
    dx <- diff(rx <- range(x, na.rm = TRUE))
    if (dx == 0)
      dx <- abs(rx[1L])
    breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000,
                      length.out = nb)
  }
  else nb <- length(breaks <- sort.int(as.double(breaks)))
  if (anyDuplicated(breaks))
    stop("'breaks' are not unique")
  codes.only <- FALSE
  if (is.null(labels)) {
    for (dig in dig.lab:max(12L, dig.lab)) {
      ch.br <- formatC(breaks, digits = dig, width = 1L)
      if (ok <- all(ch.br[-1L] != ch.br[-nb]))
        break
    }
    labels <- if (ok)
      paste0(if (right)
        "("
        else "[", ch.br[-nb], ",", ch.br[-1L], if (right)
          "]"
        else ")")
    else paste("Range", seq_len(nb - 1L), sep = "_")
    if (ok && include.lowest) {
      if (right)
        substr(labels[1L], 1L, 1L) <- "["
      else substring(labels[nb - 1L], nchar(labels[nb -
                                                     1L], "c")) <- "]"
    }
  }
  else if (is.logical(labels) && !labels)
    codes.only <- TRUE
  else if (length(labels) != nb - 1L)
    stop("lengths of 'breaks' and 'labels' differ")
  code <- .bincode(x, breaks, right, include.lowest)
  if (codes.only)
    FIN <-    code
  else FIN <- factor(code, seq_along(labels), labels, ordered = ordered_result)
  list(output = FIN, ranges = data.frame(lower = ch.br[-nb], upper = ch.br[-1L]))
}

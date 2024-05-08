#' Print function
#'
#' Print list (by type of fit statistics) of matrices of sampled statistics for a \code{RASCHstats} object.
#'
#' @param x A \code{RASCHstats} object.
#'
#' @export bystat
#'
bystat <- function(x) {
  UseMethod("bystat", x)
}
#' @rdname bystat
#' @export
bystat.RASCHstats <- function(x) {

  if (!inherits(x, "RASCHstats")) {
    stop("use only with \"RASCHstats\" objects")
  }

  extract.stat <- function(stat) lapply(x$statobj, function(y) y[[stat]])

  x.by.stat <- lapply(seq_along(x$statobj[[1]]), function(stat)
    do.call(rbind, extract.stat(stat)))
  names(x.by.stat) <- names(x$statobj[[1]])

  x.by.stat

}

#' Parameter vector to parameter matrix function
#'
#' This function transform a vector of item parameters from a polytomous Rasch model to a matrix of item parameters where rows correspond to item categories and columns correspond to items or vice versa.
#'
#' ...
#'
#' @param x Matrix of item responses.
#' @param par Vector of item parameters.
#' @param byrows Items or response levels in rows.
#'
#' @return Matrix of item parameters.
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#' thr <- PARmat(x = it.SPADI, par = model.SPADI$betapar)
#'
#' @export PARmat
#'
PARmat <- function(x, par, byrows = c("levels", "items")) {

  byrows <- match.arg(byrows)

  k <- ncol(x)
  mi <- apply(x, 2, max, na.rm = TRUE)
  cols <- do.call(c, lapply(1:k, function(i) 1:mi[i]))
  rows <- do.call(c, lapply(1:k, function(i) rep(i, mi[i])))
  thr <- matrix(NA, nrow = ncol(x), ncol = max(x))

  for (i in 1:length(par)) {
    thr[rows[i], cols[i]] <- par[i]
  }

  if(byrows == "items") thr else t(thr)
}

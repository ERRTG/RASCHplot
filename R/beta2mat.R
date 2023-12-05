#' Parameter vector to parameter matrix function
#'
#' This function transform a vector of item parameters from a polytomous Rasch model to a matrix of item parameters where rows correspond to item categories and columns correspond to items or vice versa.
#'
#' ...
#'
#' @param x Matrix (nobs x K) of K item responses.
#' @param beta Vector of item parameters.
#'
#' @return Matrix of item parameters.
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#' betamat <- beta2mat(x = it.SPADI, beta = model.SPADI$betapar)
#'
#' @export beta2mat
#'
beta2mat <- function(x, beta) {

  k <- ncol(x)
  mi <- apply(x, 2, max, na.rm = TRUE)
  cols <- do.call(c, lapply(1:k, function(i) 1:mi[i]))
  rows <- do.call(c, lapply(1:k, function(i) rep(i, mi[i])))
  betamat <- matrix(NA, nrow = ncol(x), ncol = max(x, na.rm = TRUE))

  for (i in 1:length(beta)) {
    betamat[rows[i], cols[i]] <- beta[i]
  }

  betamat
}

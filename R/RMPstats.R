#' Compute fit statistics for polytomous Rasch model
#'
#' @param delta Matrix K columns of item-category threshold parameters for K items with at most M categories and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Vector of person parameters.
#' @param dat Matrix with item responses.
#'
#' @export RMPstats
RMPstats <- function(delta, theta, dat) {

  N <- nrow(dat)
  K <- ncol(dat)
  M <- max(dat, na.rm = TRUE)            # max number of categories - 1 for items
  mi <- apply(dat, 2, max, na.rm = TRUE) # apply(delta, 1, function(x) sum(!is.na(x))) # number of categories - 1 for each item #

  E <- matrix(nrow = N, ncol = K)
  W <- matrix(nrow = N, ncol = K)
  Civ <- matrix(nrow = N, ncol = K)

  for (i in 1:K) {

    probssim <- pcmfct(delta = delta, theta = theta, ii = i)
    matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
    E[, i] <- rowSums(matx * probssim, na.rm = TRUE)
    W[, i] <- rowSums((matx - E[, i])^2 * probssim, na.rm = TRUE)
    Civ[, i] <- rowSums((matx - E[, i])^4 * probssim, na.rm = TRUE)

  }




  R <- dat - E
  Z <- R / sqrt(W) # unconditional standardised residuals
  Z[Z %in% c(Inf, -Inf)] <- NA

  outfitsim <- colSums(Z^2, na.rm = TRUE) / N
  infitsim <- colSums(R^2, na.rm = TRUE) / colSums(W, na.rm = TRUE)

  Vout <- colSums(Civ / W^2, na.rm = TRUE) / N^2 - 1 / N
  Vin <- colSums(Civ - W^2, na.rm = TRUE) / (colSums(W, na.rm = TRUE))^2

  tinfit <- (infitsim^(1/3) - 1) * 3 / sqrt(Vin) + sqrt(Vin) / 3
  toutfit <- (outfitsim^(1/3) - 1) * 3 / sqrt(Vout) + sqrt(Vout) / 3

  f <- (N * K - N - sum(mi) + 1) / K
  Y2 <- colSums(Z^2, na.rm = TRUE)

  fitresid <- (log(Y2) - log(f)) / sqrt(1 / f^2 * Vout * N^2)

  list(Outfit = outfitsim, Infit = infitsim,
       tOutfit = toutfit, tInfit = tinfit,
       FitResid = fitresid)

}

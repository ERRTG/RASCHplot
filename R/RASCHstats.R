#' Compute fit statistics
#'
#' @param beta Vector or matrix of item parameters.
#' @param theta Vector of person parameters.
#' @param dat Matrix with item responses.
#'
#' @export RASCHstats
RASCHstats <- function(beta, theta, dat) {

  N <- nrow(dat)
  K <- ncol(dat)
  M <- max(dat, na.rm = TRUE)            # max number of categories - 1 for items
  mi <- apply(dat, 2, max, na.rm = TRUE) # number of categories - 1 for each item

  #--------- Polytomous items --------------------------------------------------
  if (all(any(class(beta) %in% c("matrix", "data.frame")))) {

    if (ncol(beta) > 1) {

      E <- matrix(nrow = N, ncol = K)
      W <- matrix(nrow = N, ncol = K)
      Civ <- matrix(nrow = N, ncol = K)

      for (i in 1:K) {

        probssim <- pcmfct(theta = theta, b = beta, ii = i)
        matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
        E[, i] <- rowSums(matx * probssim, na.rm = TRUE)
        W[, i] <- rowSums((matx - E[, i])^2 * probssim, na.rm = TRUE)
        Civ[, i] <- rowSums((matx - E[, i])^4 * probssim, na.rm = TRUE)

      }

    }


    #--------- Dichotomous items -------------------------------------------------
  } else if (class(beta) %in% c("numeric", "integer")) {

    probssim <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
    E <- probssim
    W <- probssim * (1-probssim)
    Civ <- E^4 * (1 - probssim) + (1 - E)^4 * probssim

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

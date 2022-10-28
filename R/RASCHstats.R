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
  } else if (class(beta) == "numeric") {

    probssim <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
    E <- probssim
    W <- probssim * (1-probssim)
    Civ <- E^4 * (1 - probssim) + (1 - E)^4 * probssim

  }

  R <- dat - E
  Z <- R / sqrt(W) # unconditional standardised residuals
  Z[Z %in% c(Inf, -Inf)] <- NA

  outfitsim <- colSums(Z^2) / N
  infitsim <- colSums(R^2) / colSums(W)

  Vout <- colSums(Civ / W^2) / N^2 - 1 / N
  Vin <- colSums(Civ - W^2) / (colSums(W))^2

  tinfit <- (infitsim^(1/3) - 1) * 3 / Vin + Vin / 3
  toutfit <- (outfitsim^(1/3) - 1) * 3 / Vout + Vout / 3

  f <- (N * K - N - sum(mi) + 1) / K
  Y2 <- colSums(Z^2)

  fitresid <- (log(Y2) - log(f)) / sqrt(1 / f^2 * Vout * N^2)

  list(Outfit = outfitsim, Infit = infitsim,
       tOutfit = toutfit, tInfit = tinfit,
       FitResid = fitresid)

}
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

        probs <- pcmfct(theta = theta, b = beta, ii = i)
        matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
        E[, i] <- rowSums(matx * probs, na.rm = TRUE)
        W[, i] <- rowSums((matx - E[, i])^2 * probs, na.rm = TRUE)
        Civ[, i] <- rowSums((matx - E[, i])^4 * probs, na.rm = TRUE)

      }

    }


    #--------- Dichotomous items -------------------------------------------------
  } else if (class(beta) == "numeric") {

    probs <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
    E <- probs
    W <- probs * (1-probs)
    Civ <- E^4 * (1 - probs) + (1 - E)^4 * probs

  }

  R <- dat - E
  Z <- R / sqrt(W) # unconditional standardised residuals
  Z[Z %in% c(Inf, -Inf)] <- NA

  outfit <- colSums(Z^2) / N
  infit <- colSums(R^2) / colSums(W)

  Vout <- colSums(Civ / W^2) / N^2 - 1 / N
  Vin <- colSums(Civ - W^2) / (colSums(W))^2

  tinfit <- (infit^(1/3) - 1) * 3 / Vin + Vin / 3
  toutfit <- (outfit^(1/3) - 1) * 3 / Vout + Vout / 3

  f <- (N * K - N - sum(mi) + 1) / K
  Y2 <- colSums(Z^2)

  fitresid <- (log(Y2) - log(f)) / sqrt(1 / f^2 * Vout * N^2)

  list(Outfit = outfit, Infit = infit,
       tOutfit = toutfit, tInfit = tinfit,
       FitResid = fitresid)

}

#' Compute residuals from item fit for polytomous items
#'
#' @param beta Vector or matrix of item parameters.
#' @param theta Vector of person parameters.
#' @param dat Matrix with item responses.
#'
#' @export
#'
RMPresiduals <- function(beta, theta, dat) {

  if (all(any(class(beta) %in% c("matrix", "data.frame")))) {
    beta <- beta
  } else if (class(beta) == "numeric") {
    beta <- PARmat(x = dat, par = beta)
  } else {
    stop("beta is not numeric, matrix or data.frame")
  }

  N <- nrow(dat)
  K <- ncol(dat)
  M <- max(dat, na.rm = TRUE)            # max number of categories - 1 for items
  mi <- apply(dat, 2, max, na.rm = TRUE) # number of categories - 1 for each item

    if (ncol(beta) > 1) {

      E <- matrix(nrow = N, ncol = K)
      W <- matrix(nrow = N, ncol = K)
      Civ <- matrix(nrow = N, ncol = K)

      for (i in 1:K) {

        probssim <- pcmfct(theta = theta, b = beta, ii = i)
        matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
        E[, i] <- rowSums(matx * probssim, na.rm = TRUE)
        #W[, i] <- rowSums((matx - E[, i])^2 * probssim, na.rm = TRUE)
        #Civ[, i] <- rowSums((matx - E[, i])^4 * probssim, na.rm = TRUE)

      }

    }


  R <- dat - E # unconditional residuals

  class(R) <- c("data.frame","RMPresiduals")
  R

}

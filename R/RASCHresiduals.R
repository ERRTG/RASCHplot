#' Compute residuals from item fit
#'
#' @param beta A numeric vector or matrix of item parameters with compatible dimensions to data.
#' @param theta A vector of person parameters with compatible dimensions to data.
#' @param data Matrix with item responses.
#'
#' @export
#'
RASCHresiduals <- function(beta, theta, data) {

  # Check if data is dichotomous or polytomous
  if(nlevels(as.factor(as.matrix(data))) > 2) {
    type <- "RMP"
  } else {
    type <- "RMD"
  }

  # Exclude
  X.ex <- data[rowSums(data, na.rm = TRUE) != min(data, na.rm = TRUE) & rowSums(data, na.rm = TRUE) != ncol(data)*max(data, na.rm = TRUE),]

  if (type == "RMP") {

    if (all(any(class(beta) %in% c("matrix", "data.frame")))) {
      beta <- beta
    } else if (class(beta) == "numeric") {
      beta <- PARmat(x = X.ex, par = beta)
    } else {
      stop("beta is not numeric, matrix or data.frame")
    }

    N <- nrow(X.ex)
    K <- ncol(X.ex)
    M <- max(X.ex, na.rm = TRUE)            # max number of categories - 1 for items
    mi <- apply(X.ex, 2, max, na.rm = TRUE) # number of categories - 1 for each item

    if (ncol(beta) > 1) {

      E <- matrix(nrow = N, ncol = K)
      W <- matrix(nrow = N, ncol = K)
      Civ <- matrix(nrow = N, ncol = K)

      for (i in 1:K) {

        probssim <- pcmfct(theta = theta, b = beta, ii = i)
        matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
        E[, i] <- rowSums(matx * probssim, na.rm = TRUE)

      }

    }

  }

  if (type == "RMD") {

    N <- nrow(X.ex)
    K <- ncol(X.ex)
    M <- max(X.ex, na.rm = TRUE)            # max number of categories - 1 for items
    mi <- apply(X.ex, 2, max, na.rm = TRUE) # number of categories - 1 for each item

    if (class(beta) == "numeric") {

      probssim <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
      E <- probssim
      #W <- probssim * (1-probssim)
      #Civ <- E^4 * (1 - probssim) + (1 - E)^4 * probssim

    } else {
      stop("beta is not numeric")
    }

  }

  R <- X.ex - E # unconditional residuals

  class(R) <- c(class(R),"RASCHresiduals")
  R

}

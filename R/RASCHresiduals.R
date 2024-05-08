#' Compute residuals from item fit
#'
#' @param delta A numeric vector or matrix of item (-category) parameters with compatible dimensions to data.
#' @param theta A vector of person parameters with compatible dimensions to data.
#' @param data Matrix with item responses.
#' @param standardize Logical flag for standardization of residuals. Default is \code{standardize=TRUE}.
#'
#' @export
#'
RASCHresiduals <- function(delta, theta, data, standardize = TRUE) {

  # Check if data is dichotomous or polytomous
  if(nlevels(as.factor(as.matrix(data))) > 2) {
    type <- "RMP"
  } else {
    type <- "RMD"
  }

  # Check dimensions
  ntheta <- length(theta)
  ndata <- nrow(data)
  if (ntheta != ndata) {
    stop("Length of theta must equal number of rows in data.")
  }

  if (type == "RMP") {

    if (!inherits(delta, c("matrix", "data.frame"))) {
      stop("delta is not a matrix or data.frame")
    }

    N <- nrow(data)
    K <- ncol(data)
    M <- max(data, na.rm = TRUE)            # max number of categories - 1 for items
    mi <- apply(data, 2, max, na.rm = TRUE) # number of categories - 1 for each item

    E <- matrix(nrow = N, ncol = K)
    E2 <- matrix(nrow = N, ncol = K)
    W <- matrix(nrow = N, ncol = K)
    Civ <- matrix(nrow = N, ncol = K)

    for (i in 1:K) {

      probssim <- pcmfct(delta = delta, theta = theta, ii = i)
      matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
      E[, i] <- rowSums(matx * probssim, na.rm = TRUE)
      E2[, i] <- rowSums(matx^2 * probssim, na.rm = TRUE)
      W[, i] <- E2[, i] - E[, i]^2

    }



  }

  if (type == "RMD") {

    N <- nrow(data)
    K <- ncol(data)
    M <- max(data, na.rm = TRUE)            # max number of categories - 1 for items
    mi <- apply(data, 2, max, na.rm = TRUE) # number of categories - 1 for each item

    if (inherits(delta, "numeric")) {

      probssim <- sapply(1:K, function(ii) irffct(delta = delta, theta = theta, ii)[, 2])
      E <- probssim
      W <- probssim * (1-probssim)
      #Civ <- E^4 * (1 - probssim) + (1 - E)^4 * probssim

    } else {
      stop("delta is not numeric")
    }

  }

  if (standardize) {
    R <- as.matrix((data - E) / sqrt(W)) # unconditional residuals
  } else {
    R <- as.matrix(data - E) # unconditional residuals
  }


  class(R) <- c(class(R), "RASCHresiduals")
  R

}

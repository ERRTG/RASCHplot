#' Compute residuals from item fit for dichotomous items
#'
#' @param beta Vector or matrix of item parameters.
#' @param theta Vector of person parameters.
#' @param dat Matrix with item responses.
#'
#' @export
#'
RMDresiduals <- function(beta, theta, dat) {

  N <- nrow(dat)
  K <- ncol(dat)
  M <- max(dat, na.rm = TRUE)            # max number of categories - 1 for items
  mi <- apply(dat, 2, max, na.rm = TRUE) # number of categories - 1 for each item

  if (class(beta) == "numeric") {

    probssim <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])
    E <- probssim
    #W <- probssim * (1-probssim)
    #Civ <- E^4 * (1 - probssim) + (1 - E)^4 * probssim

  } else {
    stop("beta is not numeric")
  }

  R <- dat - E # unconditional residuals

  class(R) <- c("data.frame","RMDresiduals")
  R

}

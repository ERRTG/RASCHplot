#' Print a simRMstats object
#'
#' Print a summary of the simRMstats.
#'
#' @aliases print.simRMstats
#' @param x \code{simRMstats} object
#' @param \dots additional print arguments
#' @seealso \code{plot} methods.
#'
#' @method print simRMstats
#' @rdname print.simRMstats
#' @export
#'
print.simRMstats <- function(x, ...) {
    #cat("\nCall: ", deparse(x$call), "\n\n")
    out <- list(beta = x$statobj$beta,
                theta = x$statobj$theta,
                dat = x$statobj$dat)
    class(out) = c("simRMstats", class(out))
    print(out)
  }
#'
#' Item response function
#'
#' @param delta Vector of item parameters for dichotomous items.
#' @param theta Vector of person parameters.
#' @param ii item index
#'
#' @noRd
irffct <- function(delta, theta, ii){
  eta <- exp(theta - delta[ii])
  pbs <- eta / (1 + eta)
  pbs <- cbind(1 - pbs, pbs)
  return(pbs)
}
#' PCM function
#'
#' @param delta Matrix (K x (1 + M)) with location (1st column) and K columns
#' of item-category threshold parameters for K items with at most M categories
#' and number of rows equal to (maximum) number of response categories and NA
#' assigned to empty categories.
#' @param theta Vector of person parameters.
#' @param ii item index
#'
#' @references Mair, P., & Hatzinger, R. . (2007). Extended Rasch Modeling:
#' The eRm Package for the Application of IRT Models in R. Journal of
#' Statistical Software, 20(9), 1–20. https://doi.org/10.18637/jss.v020.i09
#'
#' @noRd
pcmfct <- function(delta, theta, ii){

  if (!all(any(class(delta) %in% c("matrix", "data.frame")))) {
    stop("delta is not a matrix or data.frame")
  }

  N <- length(theta)  # number of persons
  M <- ncol(delta)     # max number of categories - 1 for items

  beta <- delta2beta(delta = delta)

  beta0 <- 0#colMeans(beta)#- sum(beta[, ii]) #
  matb <- matrix(c(beta0, beta[ii, ]), nrow = N, ncol = M+1, byrow = TRUE)
  matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
  eta <- exp(theta * matx + matb)
  pbs <- eta / rowSums(eta, na.rm=TRUE)
  return(pbs)
}

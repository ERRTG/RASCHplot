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
#' @param b Vector or matrix of item parameters.
#' @param theta Vector of person parameters.
#' @param ii item index
#'
#' @noRd
irffct <- function(theta, b, ii){
  eta <- exp(theta - b[ii])
  pbs <- eta / (1 + eta)
  pbs <- cbind(1 - pbs, pbs)
  return(pbs)
}
#' PCM function
#'
#' @param b Matrix of item parameters with number of columns equal to number of items and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Vector of person parameters.
#' @param ii item index
#'
#' @references Mair, P., & Hatzinger, R. . (2007). Extended Rasch Modeling: The eRm Package for the Application of IRT Models in R. Journal of Statistical Software, 20(9), 1–20. https://doi.org/10.18637/jss.v020.i09
#'
#' @noRd
pcmfct <- function(theta, b, ii){

  if (!all(any(class(b) %in% c("matrix", "data.frame")))) {
    stop("b is not a matrix or data.frame")
  }

  N <- length(theta)  # number of persons
  M <- nrow(b)        # max number of categories - 1 for items

  beta0 <- 0# - sum(beta[, i]) #
  matb <- matrix(c(beta0, b[, ii]), nrow = N, ncol = M+1, byrow = TRUE)
  matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
  eta <- exp(theta * matx + matb)
  pbs <- eta / rowSums(eta, na.rm=TRUE)
  return(pbs)
}

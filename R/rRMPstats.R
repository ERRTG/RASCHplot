#' Sampling distribution of item fit statistics under polytomous Rasch model
#'
#' Does repeated random sampling to obtain a sampling distribution for an item fit statistic of interest.
#'
#' For more details see the help vignette:
#' \code{vignette("simRASCHstats", package = "RASCHplot")}
#'
#' @param delta Input item-category threshold parameters. Should be a matrix or data.frame with number of columns equal to number of items and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Input person parameters.
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param B Number of simulations.
#' @param trace.it If \code{trace.it=1}, then progress bars are displayed.
#'
#' @return An object with S3 class \code{"RASCHstats"}.  \item{call}{the call
#' that produced this object} \item{statobj}{List of item fit statistics.}
#' \item{method.item}{Estimation method used for item parameters}
#' \item{method.person}{Estimation method used for person parameters}
#'
#' @examples
#'
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' object <- eRm::PCM(it.SPADI)
#' pp <- eRm::person.parameter(object)
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)
#' theta <- pp$thetapar$NAgroup1
#' obj <- rRMPstats(delta, theta, method.item = "CML", method.person = "WML", B = 2)
#' plot(obj)
#'
#' @export rRMPstats
rRMPstats <- function(delta, theta, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), B, trace.it = 0){

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)


  if (!all(any(class(delta) %in% c("matrix", "data.frame")))) {
    stop("delta is not a matrix or data.frame")
  }

  N <- length(theta)
  K <- ncol(delta)

  #------------- Simulate polytomous item responses ------------------
  X <- rRMP(delta = delta, theta = theta, B = B)

  statobj <- vector(mode = "list", length = B)

  if (trace.it) cat("Simulating\n")
  for (b in 1:B) {

    #============= Fit parameters ==============================================

    fit <- RASCHfits(method.item, method.person, dat = X[[b]])
    delta.sim <- fit$delta
    theta.sim <- fit$theta

    #============= Compute fit statistics ======================================

    statobj[[b]] <- RMPstats(delta = delta.sim, theta = theta.sim, dat = X[[b]])

    if (trace.it) message(paste(b)) #cat(sprintf("Iteration: %d/%d\n", b, B))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person)

  class(stats) <- "RASCHstats"
  stats

}

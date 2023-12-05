#' Sampling distribution of item fit statistics under dichotomous Rasch model
#'
#' Does repeated random sampling to obtain a sampling distribution for an item fit statistic of interest.
#'
#' For more details see the help vignette:
#' \code{vignette("simRASCHstats", package = "RASCHplot")}
#'
#' @param delta Input item parameters. If items are dichotomous \code{delta} should be a vector.
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
#' @export rRMDstats
rRMDstats <- function(delta, theta, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), B, trace.it = 0){

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)

  N <- length(theta)
  K <- length(delta)

  #------------- Simulate item responses (0/1) -----------------------
  X <- rRMD(delta = delta, theta = theta, B = B)

  statobj <- vector(mode = "list", length = B)

  if (trace.it) cat("Simulating\n")
  for (b in 1:B) {

    #============= Fit parameters ==============================================

    fit <- RASCHfits(method.item, method.person, X[[b]])
    delta.sim <- fit$delta
    theta.sim <- fit$theta

    #============= Compute fit statistics ======================================

    statobj[[b]] <- RMDstats(delta = delta.sim, theta = theta.sim, dat = X[[b]])

    if (trace.it) message(paste(b)) #cat(sprintf("Iteration: %d/%d\n", b, B))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person)

  class(stats) <- "RASCHstats"
  stats

}

#' Simulate Yen's Q3 statistics
#'
#' Does repeated random sampling to obtain a sampling distribution for Yen's Q3 statistics.
#'
#' @param delta Input tem-category threshold parameters
#' @param theta Input person parameters
#' @param method.item Estimation method for item paramters
#' @param method.person Estimation method for person parameters
#' @param B Number of simulations
#' @param model Character string. Either "RMD" for dichotomous items or "RMP" for polytomous items.
#' @param standardize Logical flag for standardization of residuals. Default is \code{standardize=TRUE}.
#' @param trace.it If \code{trace.it=1}, then progress bars are displayed.
#'
#' @return An object with S3 class \code{"RASCHq3"}.  \item{call}{the call
#' that produced this object} \item{statobj}{List of item fit statistics.}
#' \item{method.item}{Estimation method used for item parameters}
#' \item{method.person}{Estimation method used for person parameters}
#'
#' @references Christensen, K. B., Makransky, G. and Horton, M. (2017)
#' \emph{Critical Values for Yen’s Q3: Identification of Local Dependence in the Rasch Model Using Residual Correlations, Applied Psychological Measurement, Vol. 41(3), 178-194},\cr
#' \doi{https://doi.org/10.1177/0146621616677520}.\cr
#' Yen W. M. (1984)
#' \emph{Effects of local item dependence on the fit and equating performance of the three-parameter logistic model, Applied Psychological Measurement, Vol. 8, 125-145},\cr
#' \doi{10.18637/jss.v039.i05}.
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' set.seed(1)
#' object <- eRm::PCM(it.SPADI)
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)
#' pp <- eRm::person.parameter(object)
#' theta <- unlist(pp$thetapar)
#' sims <- simRASCHq3(delta, theta,
#'                    method.item = "JML", method.person = "MLE",
#'                    B = 10, model = "RMP")
#'
#' @export simRASCHq3
simRASCHq3 <- function(delta, theta, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), B, model = c("RMD", "RMP"), standardize = TRUE, trace.it = 0){

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)
  model <- match.arg(model)

  if (model == "RMP") {

    N <- length(theta)
    K <- ncol(delta)

    #------------- Simulate polytomous item responses ------------------
    X <- rRMP(delta = delta, theta = theta, B = B)

  }
  if (model == "RMD") {

    N <- length(theta)
    K <- length(beta)

    #------------- Simulate item responses (0/1) -----------------------
    X <- rRMD(delta = delta, theta = theta, B = B)

  }

  statobj <- vector(mode = "list", length = B)

  if (trace.it) cat("Simulating\n")
  for (b in 1:B) {

    #============= Fit item parameters =================================

    fit <- RASCHfits(method.item, method.person, X[[b]])
    delta.sim <- fit$delta
    theta.sim <- fit$theta

    #============= Compute Q3 statistics =======================================

    resids <- RASCHresiduals(delta = delta.sim, theta = theta.sim, data = X[[b]])
    fitQ3 <- Q3(items = X[[b]], method.item = method.item, method.person = method.person,
                model = model, standardize = standardize)
    statobj[[b]] <- fitQ3$Q3matrix #max(fitQ3) - mean(fitQ3)

    if (trace.it) message(paste(b)) #cat(sprintf("Iteration: %d/%d\n", b, B))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person)

  class(stats) <- "RASCHq3"
  stats

}

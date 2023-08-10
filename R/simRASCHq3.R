#' Simulate Yen's Q3
#'
#' Does repeated random sampling to obtain a sampling distribution for Yen's Q3
#'
#' @param beta Input item parameters
#' @param theta Input person parameters
#' @param method.item Estimation method for item paramters
#' @param method.person Estimation method for person parameters
#' @param B Number of simulations
#' @param model Character string. Either "RMD" for dichotomous items or "RMP" for polytomous items.
#' @param trace.it If \code{trace.it=1}, then progress bars are displayed.
#'
#' @return An object with S3 class \code{"RASCHq3"}.  \item{call}{the call
#' that produced this object} \item{statobj}{List of item fit statistics.}
#' \item{method.item}{Estimation method used for item parameters}
#' \item{method.person}{Estimation method used for person parameters}
#'
#' @export simRASCHq3
simRASCHq3 <- function(beta, theta, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), B, model = c("RMD", "RMP"), trace.it = 0){

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)
  model <- match.arg(model)

  if (model == "RMP") {

    N <- length(theta)
    K <- ncol(beta)
    M <- nrow(beta)
    mi <- rep(nrow(beta), K) #apply(beta, 2, function(x) sum(!is.na(x)))

    #------------- Simulate polytomous item responses ------------------
    X <- simRASCHdata(model, theta, beta, B, M, mi)

  }
  if (model == "RMD") {

    N <- length(theta)
    K <- length(beta)

    #------------- Simulate item responses (0/1) -----------------------
    X <- simRASCHdata(model = "RMD", theta, beta, B)

  }

  statobj <- vector(mode = "list", length = B)

  if (trace.it) cat("Simulating\n")
  for (b in 1:B) {

    #============= Fit item parameters =================================

    fit <- RASCHfits(method.item, method.person, X[[b]])
    beta.sim <- fit$beta
    theta.sim <- fit$theta

    while (all(is.na(beta.sim))) {
      X[[b]] <- simResps(model, probs, B = 1, M, mi)[[1]]
      fit <- RASCHfits(method.item, method.person, dat = X[[b]])
      beta.sim <- fit$beta
      theta.sim <- fit$theta
    }

    #============= Compute Q3 statistics =======================================

    resids <- RASCHresiduals(beta = beta.sim, theta = theta.sim, data = X[[b]])
    fitQ3 <- Q3(resids)
    statobj[[b]] <- fitQ3 #max(fitQ3) - mean(fitQ3)

    if (trace.it) message(paste(b)) #cat(sprintf("Iteration: %d/%d\n", b, B))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person)

  class(stats) <- "RASCHq3"
  stats

}

#' Simulate item responses
#'
#' Does repeated random sampling to obtain a sampling distribution for an item fit statistic of interest.
#'
#' For more details see the help vignette:
#' \code{vignette("simRASCHstats", package = "RASCHplot")}
#'
#' @param beta Input item parameters. If items are dichotomous \code{beta} should be a vector. If items a polytomous \code{beta} should be a matrix or data.frame with number of columns equal to number of items and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Input person parameters.
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param B Number of simulations.
#' @param model Character string. Either "RMD" for dichotomous items or "RMP" for polytomous items.
#' @param trace.it If \code{trace.it=1}, then progress bars are displayed.
#'
#' @return An object with S3 class \code{"RASCHstats"}.  \item{call}{the call
#' that produced this object} \item{statobj}{List of item fit statistics.}
#' \item{method.item}{Estimation method used for item parameters}
#' \item{method.person}{Estimation method used for person parameters}
#'
#' @export simRASCHstats
simRASCHstats <- function(beta, theta, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), B, model = c("RMD", "RMP"), trace.it = 0){

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)
  model <- match.arg(model)

  if (model == "RMP") {

    if (all(any(class(beta) %in% c("matrix", "data.frame")))) {
      beta <- beta
    } else {
      stop("beta is not a matrix or data.frame")
    }

    N <- length(theta)
    K <- ncol(beta)
    M <- nrow(beta)
    mi <- apply(beta, 2, function(x) sum(!is.na(x))) #rep(nrow(beta), K) #

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

    #============= Compute fit statistics ========================================

    statobj[[b]] <- RASCHstats(beta = beta.sim, theta = theta.sim, dat = X[[b]])

    if (trace.it) message(paste(b)) #cat(sprintf("Iteration: %d/%d\n", b, B))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person)

  class(stats) <- "RASCHstats"
  stats

}

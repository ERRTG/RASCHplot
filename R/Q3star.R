#' Compute Yen's Q3 statistics
#'
#' This function computes Yen's Q3 for selected items in a Rasch-model. This can be used to investigate item misfit.
#'
#' Yen's Q3 statistic can be used to detect local dependence.
#'
#' @param items Input data matrix or data frame; rows represent indiviiduals, columns represent items. Or a model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()} or \code{PCM()} from the \code{eRm} package.
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param model Character string. Either "RMD" for dichotomous items or "RMP" for polytomous items.
#' @param standardize Logical flag for standardization of residuals, prior to
#' computing Yen's Q3. Default is \code{standardize=TRUE}.
#' @param ... Arguments to be passed to \code{stats::cor}.
#'
#' @return An object with S3 class \code{"Q3star"}.  \item{call}{the call
#' that produced this object} \item{statobj}{List of Q3 statistics.}
#' \item{method.item}{Estimation method used for item parameters}
#' \item{method.person}{Estimation method used for person parameters}
#'
#' @references Christensen, K. B., Makransky, G. and Horton, M. (2017)
#' \emph{Critical Values for Yen’s Q3: Identification of Local Dependence in the Rasch Model Using Residual Correlations, Applied Psychological Measurement, Vol. 41(3), 178-194},
#' \doi{https://doi.org/10.1177/0146621616677520}.\cr
#' Yen W. M. (1984)
#' \emph{Effects of local item dependence on the fit and equating performance of the three-parameter logistic model, Applied Psychological Measurement, Vol. 8, 125-145},
#' \doi{10.18637/jss.v039.i05}.
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' q3obj <- Q3star(items = it.SPADI, method.item = "CML", method.person = "WML", model = "RMP")
#'
#' @export Q3star
#'
Q3star <- function(items, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), model = c("RMD", "RMP"), standardize = TRUE, ...) {

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)

  #============= Fit parameters ==============================================

  fit <- RASCHfits(method.item, method.person, dat = items)

  #============= Compute residuals ==========================================

  resids  <- RASCHresiduals(fit$beta, fit$theta, items, standardize)

  #============= Compute Q3 statistics ======================================

  Q3matrix <- Q3(object = resids, ...)
  Q3max <- max(Q3matrix[upper.tri(Q3matrix, diag = FALSE)])
  Q3star <- Q3max - mean(Q3matrix[upper.tri(Q3matrix, diag = FALSE)])

  out <- list(Q3matrix = Q3matrix, Q3max = Q3max, Q3star = Q3star)
  class(out) <- c(class(out),"Q3star")

  out

}

#' Transformation of beta parameters to delta (item-category threshold) parameters and vice versa
#'
#' \code{beta2delta} transforms the \eqn{\beta}{beta} parameters in \deqn{P(X_{vi} = x) = \frac{\exp(x\theta_v + \beta_{ix})}{\sum_{l=0}^{m_i}\exp(l\theta_v + \beta_{il})}}{P(X_{vi} = x) = (exp(x*theta_v + beta_{ix}))/(sum_{l=0}^{m_i} exp(l*theta_v + beta_{il}))} into delta (item-category threshold) parameters.
#' \code{delta2beta} transforms the \eqn{delta}{delta} (item-category threshold) parameters in \deqn{P(X_{vi} = x) = \frac{\exp(x\theta_v - \sum_{h=1}^x\delta_{il})}{\sum_{l=0}^{m_i}\exp(l\theta_v - \sum_{h=l}^x\delta_{il})}}{P(X_{vi} = x) = (exp(x*theta_v - sum_{h=1}^x delta_{il}))/(sum_{l=0}^{m_i} exp(l*theta_v - sum_{h=l}^x delta_{il}))} into delta (item-category threshold) parameters.
#'
#' @param beta Matrix (K x M) or vector (K*M) of beta parameters for K items with at most M categories.
#' @param delta Matrix with K x M item-category threshold parameters for K items with at most M categories.
#' @param x Matrix (nobs x K) of K item responses. Only used if \code{beta} is a vector to transform \code{beta} to a matrix.
#'
#' @return Matrix of item parameters.
#'
#' @references Mair, P., & Hatzinger, R. . (2007). Extended Rasch Modeling: The eRm Package for the Application of IRT Models in R. Journal of Statistical Software, 20(9), 1–20. https://doi.org/10.18637/jss.v020.i09
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#' beta <- model.SPADI$betapar
#'
#' betamat <- beta2mat(x = it.SPADI, beta = model.SPADI$betapar)
#' delta <- beta2delta(beta = betamat)
#' delta.eRm <- eRm::thresholds(model.SPADI)
#'
#' betaTjek <- delta2beta(delta = delta) # Tjek mod betamat
#'
#'
#' @export beta2delta
#'
beta2delta <- function(beta, x) {

  if (is.null(dim(beta))) {
    if (missing(x)) {
      stop("Since beta is a vector, x is needed.")
    } else {
      betamat <- beta2mat(x = x, beta = beta)
    }
  } else {
    betamat <- beta
  }

  betamat0 <- cbind(0, betamat)
  betamat0 <- betamat0[,-ncol(betamat0)]
  delta <- betamat0 - betamat
  colnames(delta) <- paste0("Threshold ", 1:ncol(betamat))
  #delta <- cbind(rowMeans(delta), delta)
  #colnames(delta) <- c("Location", paste0("Threshold ", 1:ncol(betamat)))
  if (is.null(colnames(betamat))) {
    rownames(delta) <- paste0("Item ", 1:nrow(betamat))
  } else {
    rownames(delta) <- rownames(betamat)
  }

  delta

}
#'
#' @describeIn beta2delta Transformation of delta (item-category threshold) parameters to beta parameters
#'
#' @export delta2beta
#'
delta2beta <- function(delta, x) {

  deltamat <- delta#[,-1]
  beta <- cbind(0, deltamat)
  beta <- t(apply(beta, 1, function(x) -cumsum(x)))[,-1]

  colnames(beta) <- NULL
  if (is.null(rownames(delta))) {
    rownames(beta) <- paste0("Item ", 1:nrow(delta))
  } else {
    rownames(beta) <- rownames(delta)
  }

  beta

}






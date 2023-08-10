#' Simulate Data under a Rasch model
#'
#' Repeated random sampling to obtain item responses from estimated item and person parameters.
#'
#' @param model Character. "RMD" for dichotomous Rasch model. "RMP" for partial credit model.
#' @param beta Input item parameters. If items are dichotomous \code{beta} should be a vector. If items a polytomous \code{beta} should be a matrix or data.frame with number of columns equal to number of items and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Input person parameters.
#' @param B Number of simulations.
#' @param M Maximum (over all items) number of categories. Only if `model = "RMP"`.
#' @param mi Vector of maximum number of categories per item. Only if `model = "RMP"`.
#'
#' @export simRASCHdata
#'
simRASCHdata <- function(model = c("RMD", "RMP"), theta, beta, B, M = NULL, mi = NULL) {

  model <- match.arg(model)

  X <- vector(mode = "list", length = B)

  for (b in 1:B) {

    #------------- Simulate polytomous item responses --------------------------
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

      #-------------------- Compute probabilities ----------------------------
      probs <- vector(mode = "list", length = K)
      for (i in 1:K) {
        probs[[i]] <- pcmfct(theta = theta, b = beta, ii = i)
      }

      #------------- Simulate polytomous item responses ------------------
      X <- simResps(model, probs, B, M, mi)

    }
    #------------- Simulate item responses (0/1) -------------------------------
    if (model == "RMD") {
      N <- length(theta)
      K <- length(beta)

      #-------------------- Compute probabilities ----------------------------
      probs <- sapply(1:K, function(ii) irffct(theta = theta, b = beta, ii)[, 2])

      #------------- Simulate item responses (0/1) -----------------------
      X <- simResps(model, probs, B)

    }

    colnames(X[[b]]) <- paste0("I", 1:K)
  }
  return(X)
}

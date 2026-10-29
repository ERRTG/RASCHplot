#' Simulate item responses under a polytomous Rasch model
#'
#' Repeated random sampling to obtain item responses from estimated item and person parameters.
#'
#' @param delta Matrix (K x (1 + M)) with location (1st column) and K columns of item-category threshold parameters for K items with at most M categories and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Input person parameters.
#' @param B Number of simulations.
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#'
#' betamat <- beta2mat(x = it.SPADI, beta = model.SPADI$betapar)
#' delta <- beta2delta(beta = betamat)
#' theta <- rnorm(1000)
#' obj <- rRMP(delta = delta, theta = theta, B = 2)
#' fit <- eRm::PCM(obj[[1]])
#' thresholds(fit)
#'
#' @export rRMP
#'
rRMP <- function(delta, theta, B) {

  if (!all(any(class(delta) %in% c("matrix", "data.frame")))) {
    stop("delta is not a matrix or data.frame")
  }
  if (anyNA(theta)) {
    stop("theta contains NA (undefined person estimates); remove those persons before simulating")
  }
  delta <- as.matrix(delta)

  N <- length(theta)
  K <- nrow(delta)
  mi <- apply(delta, 1, function(x) sum(!is.na(x)))   # number of categories - 1 per item
  max_total <- sum(mi)                                 # largest possible total score

  #-------------------- Compute probabilities ----------------------------
  probs <- vector(mode = "list", length = K)
  for (i in 1:K) {
    probs[[i]] <- pcmfct(delta = delta, theta = theta, ii = i)
  }

  X <- vector(mode = "list", length = B)

  for (b in 1:B) {

    #------------- Simulate polytomous item responses --------------------------
    # Redraw the whole replicate until no item is constant (as before), starting
    # from a fresh matrix each time; extreme scorers are removed.
    repeat {
      Xb <- matrix(NA, nrow = N, ncol = K)
      for (i in 1:K) {
        cumprop <- t(apply(probs[[i]], 1, cumsum))
        U <- runif(N)
        for (x in 0:mi[i]) {
          Xb[, i] <- ifelse(is.na(Xb[, i]) & U <= cumprop[, x + 1], x, Xb[, i])
        }
        Xb[is.na(Xb[, i]), i] <- mi[i]   # guard against rounding: cumulative probability sums to 1
      }
      Xb <- Xb[!(rowSums(Xb) %in% c(0, max_total)), , drop = FALSE]
      if (nrow(Xb) > 1 && !any(apply(Xb, 2, function(x) var(x) == 0))) break
    }

    colnames(Xb) <- paste0("I", 1:K)
    X[[b]] <- Xb
  }
  return(X)
}

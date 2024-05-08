#' Simulate item responses under a polytomous Rasch model
#'
#' Repeated random sampling to obtain item responses from estimated item and person parameters.
#'
#' @param delta Matrix K columns of item-category threshold parameters for K items with at most M categories and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.
#' @param theta Input person parameters.
#' @param B Number of simulations.
#'
#' @export rRMP
#'
rRMP <- function(delta, theta, B) {

  N <- length(theta)
  K <- ncol(delta)
  M <- nrow(delta)
  mi <- apply(delta, 2, function(x) sum(!is.na(x)))

  X <- vector(mode = "list", length = B)
  X <- lapply(X, function(x) matrix(nrow = N, ncol = K))

  for (b in 1:B) {

    #------------- Simulate polytomous item responses --------------------------

    if (!all(any(class(delta) %in% c("matrix", "data.frame")))) {
      stop("delta is not a matrix or data.frame")
    }

    #-------------------- Compute probabilities ----------------------------
    probs <- vector(mode = "list", length = K)
    for (i in 1:K) {
      probs[[i]] <- pcmfct(delta = delta, theta = theta, ii = i)
    }

    #------------- Simulate polytomous item responses --------------------------
    novar <- TRUE
    while (novar) {
      PP <- matrix(runif(n = N * K, 0, 1), nrow = N, ncol = K)
      for (i in 1:K) {
        for (j in 1:N) {
          cumprop <- cumsum(probs[[i]][j, ])
          U <- PP[j,i] #PP[[b]][j, i]
          for (x in 1:(mi[i]+1)) {
            if (all(is.na(cumprop))) {
              break
            } else {
              if (U <= cumprop[x]) {
                X[[b]][j,i] <- x-1
                break
              }
            }
          }
        }
      }
      #------------- Remove obs. with extreme scores -----------------------------
      test <- which(rowSums(X[[b]]) %in% c(0, M*K))
      if (!identical(test, integer(0))) {
        X[[b]] <- X[[b]][-test,]
      }
      if (all(is.na(cumprop))) {
        novar <- TRUE
      } else {
        novar <- any(apply(X[[b]], 2, function(x) var(x) == 0))
      }

    }

    colnames(X[[b]]) <- paste0("I", 1:K)
  }
  return(X)
}

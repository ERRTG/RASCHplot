#' Response simulation
#'
#' Repeated random sampling to obtain item responses from estimated item and person parameters.
#'
#' @param model Character. "RMD" for dichotomous Rasch model. "RMP" for partial credit model.
#' @param probs List or matrix of probabilities.
#' @param B Number of simulations.
#' @param M Maximum (over all items) number of categories. Only if `model = "RMP"`.
#' @param mi Vector of maximum number of categories per item. Only if `model = "RMP"`.
#'
#' @export simResps
#'
simResps <- function(model = c("RMD", "RMP"), probs, B, M = NULL, mi = NULL) {

  model <- match.arg(model)

  K <- ifelse(model == "RMD", ncol(probs), length(probs))
  N <- ifelse(model == "RMD", nrow(probs), nrow(probs[[1]]))

  X <- vector(mode = "list", length = B)

  for (b in 1:B) {

    #------------- Simulate polytomous item responses --------------------------
    if (model == "RMP") {

      novar <- TRUE
      while (novar) {
        PP <- matrix(runif(n = N * K, 0, 1), nrow = N, ncol = K)
        X[[b]] <- matrix(nrow = N, ncol = K)
        for (i in 1:K) {
          for (j in 1:N) {
            cumprop <- cumsum(probs[[i]][j, ])
            U <- PP[j,i] #PP[[b]][j, i]
            for (x in 1:(mi[i]+1)) {
              if (U <= cumprop[x]) {
                X[[b]][j,i] <- x-1
                break
              }
            }
          }
        }
        #------------- Remove obs. with extreme scores -----------------------------
        test <- which(rowSums(X[[b]]) %in% c(0, M*K))
        if (!identical(test, integer(0))) {
          X[[b]] <- X[[b]][-test,]
        }
        novar <- any(apply(X[[b]], 2, function(x) var(x) == 0))
      }
    }
    #------------- Simulate item responses (0/1) -------------------------------
    if (model == "RMD") {
      novar <- TRUE
      while (novar) {
        PP <- matrix(runif(n = N * K, 0, 1), nrow = N, ncol = K)
        X[[b]] <- matrix(nrow = N, ncol = K)
        for (i in 1:N) {
          for (j in 1:K) {
            U <- PP[i,j] #PP[[b]][j, i]
            X[[b]][i,j] <- ifelse(U > probs[i,j], 1, 0)
          }
        }
        #------------- Remove obs. with extreme scores -----------------------------
        #test <- which(rowSums(X[[b]]) %in% c(0,ncol(X[[b]])))
        #if (!identical(test, integer(0))) {
        #  X[[b]] <- X[[b]][-test,]
        #}
        #------------- Check for extreme scores -------------------------
        novar <- any(apply(X[[b]], 2, function(x) var(x) == 0))
      }
    }

    colnames(X[[b]]) <- paste0("I", 1:K)
  }
  return(X)
}

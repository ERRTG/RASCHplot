#' Simulate item responses under a dichotomous Rasch model
#'
#' Repeated random sampling to obtain item responses from estimated item and person parameters.
#'
#' @param delta Input vector of item parameters.
#' @param theta Input vector person parameters.
#' @param B Number of simulations.
#'
#' @export rRMD
#'
rRMD <- function(delta, theta, B) {

  X <- vector(mode = "list", length = B)

  for (b in 1:B) {

    #------------- Simulate item responses (0/1) -------------------------------

    N <- length(theta)
    K <- length(delta)

    #-------------------- Compute probabilities ----------------------------
    probs <- sapply(1:K, function(ii) irffct(delta = delta, theta = theta, ii)[, 2])

    #------------- Simulate item responses (0/1) -------------------------------
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

    colnames(X[[b]]) <- paste0("I", 1:K)
  }
  return(X)
}

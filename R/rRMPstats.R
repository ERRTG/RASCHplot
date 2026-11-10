#' Sampling distribution of item fit statistics under polytomous Rasch model
#'
#' Does repeated random sampling to obtain a sampling distribution for an item fit statistic of interest.
#'
#' For more details see the help vignette:
#' \code{vignette("simRASCHstats", package = "RASCHplot")}
#'
#' @param delta Input item-category threshold parameters. Should be a matrix or data.frame with one row per item and one column per threshold (number of columns equal to the maximum number of response categories minus one), with NA assigned to empty categories.
#' @param theta Input person parameters.
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param B Number of simulations.
#' @param trace.it If \code{trace.it=1}, then progress bars are displayed.
#' @param max_retries Maximum number of attempts for one replicate. A replicate fails if fitting errors or a person estimate is undefined; it is then redrawn. If all attempts fail, an error is raised.
#' @param timeout_secs Wall-clock limit in seconds for one attempt. Default \code{Inf} (no limit).
#'
#' @return An object with S3 class \code{"RASCHstats"}.  \item{call}{the call
#' that produced this object} \item{statobj}{List of item fit statistics.}
#' \item{method.item}{Estimation method used for item parameters}
#' \item{method.person}{Estimation method used for person parameters}
#' \item{attempts}{Number of attempts needed for each replicate}
#' \item{lost_category}{Logical; TRUE if a replicate lost a response category for some item}
#' \item{n}{Number of simulated persons per replicate (after removing extreme scorers)}
#'
#' @examples
#'
#' library(ggplot2)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' object <- eRm::PCM(it.SPADI)
#' pp <- eRm::person.parameter(object)
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)
#' theta <- pp$thetapar$NAgroup1
#' obj <- rRMPstats(delta, theta, method.item = "CML", method.person = "WML", B = 20)
#' plot(obj)
#'
#' fitres <- RMPstats(delta, theta, dat = it.SPADI[-pp$pers.ex,])
#' outfits <- data.frame(x = fitres$Outfit, y = 0)
#' plot(obj) +
#' geom_point(data = outfits,
#'            colour = "white", size = 1.5)
#' @export rRMPstats
rRMPstats <- function(delta, theta, method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), B, trace.it = 0,
                      max_retries = 10, timeout_secs = Inf){

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)


  if (!all(any(class(delta) %in% c("matrix", "data.frame")))) {
    stop("delta is not a matrix or data.frame")
  }
  delta <- as.matrix(delta)
  mi_model <- apply(delta, 1, function(x) sum(!is.na(x)))

  statobj <- vector(mode = "list", length = B)
  attempts <- integer(B)
  lost_category <- logical(B)
  n_sim <- integer(B)

  if (trace.it) cat("Simulating\n")
  for (b in 1:B) {

    attempt <- 0
    repeat {
      attempt <- attempt + 1
      # A replicate that hangs (e.g. eRm::PCM spinning on a quasi-separated sample)
      # is cut off after timeout_secs and redrawn like any other failure. Note that
      # setTimeLimit() only interrupts R code, not a long compiled call.
      setTimeLimit(elapsed = timeout_secs, transient = TRUE)
      result <- tryCatch({
        #============= Simulate one polytomous data set =========================
        Xb <- rRMP(delta = delta, theta = theta, B = 1)[[1]]

        #============= Fit parameters ===========================================
        fit <- suppressWarnings(RASCHfits(method.item, method.person, dat = Xb))
        if (anyNA(fit$theta)) stop("undefined person estimate in a simulated data set")

        #============= Compute fit statistics ===================================
        # pcMfits() shifts categories so each item's lowest is 0; use the same
        # shifted data here so the statistics match the fitted thresholds
        Xs <- sweep(Xb, 2, apply(Xb, 2, min))
        st <- RMPstats(delta = t(fit$delta), theta = fit$theta, dat = Xs)

        list(st = st, n = nrow(Xb),
             lost = any(apply(Xb, 2, function(x) length(unique(x))) < mi_model + 1))
      }, error = function(e) e)
      setTimeLimit(elapsed = Inf, transient = TRUE)

      if (!inherits(result, "error")) break
      if (attempt >= max_retries) {
        stop(sprintf("Replicate %d failed after %d attempts; last error: %s",
                     b, attempt, conditionMessage(result)), call. = FALSE)
      }
    }

    statobj[[b]] <- result$st
    n_sim[b] <- result$n
    lost_category[b] <- result$lost
    attempts[b] <- attempt

    if (trace.it) message(paste0(b, " (", attempt, " attempt", if (attempt > 1) "s", ")"))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person,
                attempts = attempts,
                lost_category = lost_category,
                n = n_sim)

  class(stats) <- "RASCHstats"
  stats

}

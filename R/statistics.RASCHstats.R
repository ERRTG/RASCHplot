#' Extract the item fit statistics from a RASCHstats object
#'
#' A RASCHstats object has the component \code{statobj} which is a list
#' of item fit statistics returned by the RASCHstats function for each sample.
#' \code{statistics} method returns a list of matrices of the item fit
#' statistics. Each row corresponds to an item and each column corresponds to
#' a sample.
#'
#' @param object RASCHstats object
#' @param \dots additional print arguments
#'
#' @return \code{statistics.RASCHstats} produces a list of matrices of fit statistics
#'
#' @seealso \code{RASCHstats}, \code{simRASCHstats}, and \code{plot}
#' methods.
#'
#' @export statistics.RASCHstats
statistics.RASCHstats <- function(object, ...){

  outfit <- do.call(cbind, lapply(seq_along(object$statobj),
                                  function(x) object$statobj[[x]]$Outfit))
  infit <- do.call(cbind, lapply(seq_along(object$statobj),
                                  function(x) object$statobj[[x]]$Infit))
  fitresid <- do.call(cbind, lapply(seq_along(object$statobj),
                                 function(x) object$statobj[[x]]$FitResid))
  toutfit <- do.call(cbind, lapply(seq_along(object$statobj),
                                  function(x) object$statobj[[x]]$tOutfit))
  tinfit <- do.call(cbind, lapply(seq_along(object$statobj),
                                 function(x) object$statobj[[x]]$tInfit))

  list(Outfit = outfit, Infit = infit,
       FitResid = fitresid,
       tOutfit = toutfit, tInfit = tinfit)
}

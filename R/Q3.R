#' Compute Yen's Q3 from item fit
#'
#' @param object Object of class \code{RASCHresiduals}.
#' @param ... Other arguments.
#'
#' @export Q3
#'
Q3 <- function(object, ...) {

  if (!inherits(object, c("RMDresiduals", "RMPresiduals"))) {
    stop("use only with \"RASCHresiduals\" objects")
  }

  Q3 <- cor(object, method = "pearson", use = "pairwise.complete.obs")

  Q3

}

#' Model fit for Rasch model
#'
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param dat Matrix of item responses.
#'
#' @export RASCHfits
RASCHfits <- function(method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), dat) {

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)

  if (all(range(dat, na.rm = TRUE) == c(0,1))) {
    model <- "RMD"
  } else {
    model <- "RMP"
  }

  #============= Fit item parameters =================================

  fit <- switch(model,
                "RMD"= rMfits(method.item, method.person, dat),
                "RMP" = pcMfits(method.item, method.person, dat))

  fit
}

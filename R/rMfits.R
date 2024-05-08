#' Model fit for dichotomous Rasch model
#'
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param dat Matrix of item responses.
#'
#' @export rMfits
rMfits <- function(method.item = c("PCML", "CML", "JML", "MML"), method.person = c("WML", "MLE"), dat) {

  method.item <- match.arg(method.item)
  method.person <- match.arg(method.person)

  K <- ncol(dat)

  #============= Fit item parameters =================================

  if (method.item == "CML") {
    item.fit <- eRm::RM(X = dat, sum0 = TRUE)
    delta.sim <- - item.fit$betapar
    cl <- call("eRm::RM", X = dat, sum0 = TRUE)
  }
  if (method.item == "JML") {
    item.fit <- TAM::tam.jml(resp = dat,
                        #adj = jml.adj,
                        constraint = "items",
                        verbose = FALSE)
    cl <- call("TAM::tam.jml", resp = dat, constraint = "items", verbose = FALSE)
    delta.sim <- c(item.fit$xsi, -sum(item.fit$xsi))
  }
  if (method.item == "MML") {
    item.fit <- TAM::tam.mml(resp = dat, verbose = FALSE)
    delta.sim <- item.fit$xsi$xsi - mean(item.fit$xsi$xsi)
    cl <- call("TAM::tam.mml", resp = dat, verbose = FALSE)
  }
  if (method.item == "PCML") {
    item.fit <- sirt::rasch.pairwise(dat = dat, zerosum = TRUE)
    tmp <- which(!colnames(dat) %in% item.fit$item$item)
    if (identical(tmp, integer(0))) {
      delta.sim <- item.fit$item$b   # extract item difficulties
    } else {
      nottmp <- (1:K)[-tmp]
      delta.sim <- rep(NA, K)
      delta.sim[nottmp] <- item.fit$item$b   # extract item difficulties
    }
    cl <- call("sirt::rasch.pairwise", dat = dat, zerosum = TRUE)
  }

  #============= End fit item parameters =============================

  #============= Fit person parameters: WML/MLE ======================

  if (!is.null(method.person)) {
    arg.list <- list("b" = delta.sim)

    irffct0 <- function(theta, b, ii){
      eta <- exp(theta - b[ii])
      pbs <- eta / (1 + eta)
      pbs <- cbind(1 - pbs, pbs)
      return(pbs)
    }

    person.fit <- sirt::IRT.mle(data = dat, irffct = irffct0, arg.list = arg.list,
                                type = method.person, progress = FALSE)
    theta.sim <- person.fit$est
    theta.sim[theta.sim %in% c(Inf, -Inf)] <- NA
  } else {
    theta.sim <- NULL
    person.fit <- NULL
  }


  #============= End fit person parameters ===========================

  out <- list(delta = delta.sim, theta = theta.sim,
              item.fit = item.fit, person.fit = person.fit)
  out$call <- cl

  out

}

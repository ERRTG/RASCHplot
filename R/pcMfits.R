#' Model fit for polytomous Rasch model
#'
#' @param method.item Estimation method for item parameters.
#' @param method.person Estimation method for person parameters.
#' @param dat Matrix of item responses.
#'
#' @return An object with S3 class \code{"RASCHfits"}.  \item{call}{the call that produced this
#' object}
#'
#' @export pcMfits
pcMfits <- function(method.item, method.person, dat) {

  #============= Data check as in eRm package ========================
  ri.min <- apply(dat,2,min,na.rm=TRUE)                     #if no 0 responses
  if(any(ri.min > 0)){
    warning(paste0(
      "\n",
      paste("The following items have no 0-responses:"),
      "\n",
      paste(colnames(dat)[ri.min > 0], collapse=" "),
      "\n",
      paste("Responses are shifted such that lowest category is 0.")
    ), call. = FALSE, immediate.=TRUE)
  }
  dat <- t(apply(dat,1,function(y) {y-ri.min}))           #shift down to 0
  #===================================================================

  mi <- apply(dat, 2, max, na.rm = TRUE) # number of categories - 1 for each item
  K <- ncol(dat)                         # number of items

  #============= Fit item parameters =================================

  if (method.item == "CML") {

    item.fit <- eRm::PCM(dat, sum0 = TRUE)
    cl <- call("eRm::PCM", X = dat, sum0 = TRUE)

    beta.vec <- item.fit$betapar
    rows <- do.call(c, lapply(1:K, function(i) 1:mi[i]))
    cols <- do.call(c, lapply(1:K, function(i) rep(i, mi[i])))
    #beta.sim <- matrix(NA, nrow = max(dat), ncol = ncol(dat))
    #for (i in 1:length(beta.vec)) {
    #  beta.sim[rows[i], cols[i]] <- beta.vec[i]
    #}
    beta.sim <- t(beta2mat(x = dat, beta = beta.vec))
  }
  if (method.item == "JML") {
    item.fit <- TAM::tam.jml(resp = dat,
                        #adj = jml.adj,
                        constraint = "items",
                        verbose = FALSE)
    cl <- call("TAM::tam.jml", resp = dat, constraint = "items", verbose = FALSE)

    beta.sim <- t(item.fit$item[, grep("AXsi", colnames(item.fit$item))])
    beta.sim <- beta.sim - mean(beta.sim, na.rm = TRUE) #---------------------------?
  }
  if (method.item == "MML") {
    #library(ltm)
    #item.fit <- ltm::gpcm(dat, constraint = "rasch")
    #beta.tmp <- lapply(item.fit$coefficients, function(i) i[-length(i)])
    #beta.sim <- do.call(cbind, beta.tmp)
    #beta.sim <- beta.sim
    item.fit <- TAM::tam.mml(resp = dat,
                        #constraint = "items", #------------------------------------?
                        irtmodel = "PCM", #-----------------------------------------?
                        verbose = FALSE)
    cl <- call("TAM::tam.mml", resp = dat, irtmodel = "PCM", verbose = FALSE)

    beta.vec <- item.fit$xsi$xsi #- mean(item.fit$xsi$xsi, na.rm = TRUE)
    rows <- do.call(c, lapply(1:K, function(i) 1:mi[i]))
    cols <- do.call(c, lapply(1:K, function(i) rep(i, mi[i])))
    beta.sim <- matrix(NA, nrow = max(dat), ncol = ncol(dat))
    for (i in 1:length(beta.vec)) {
      beta.sim[rows[i], cols[i]] <- beta.vec[i]
    }
  }
  if (method.item == "PCML") {
    #item.fit <- sirt::rasch.pairwise(dat, zerosum=TRUE)
    #beta.vec <- item.fit$item$b   # extract item difficulties

    item.fit <- sirt::rasch.evm.pcm(dat = dat)
    cl <- call("sirt::rasch.evm.pcm", dat = dat)

    beta.vec <- item.fit$item$est
    rows <- do.call(c, lapply(1:K, function(i) 1:mi[i]))
    cols <- do.call(c, lapply(1:K, function(i) rep(i, mi[i])))
    beta.sim <- matrix(NA, nrow = max(dat), ncol = ncol(dat))
    for (i in 1:length(beta.vec)) {
      beta.sim[rows[i], cols[i]] <- beta.vec[i]
    }
    if (all(is.na(beta.sim))) {
      theta.sim <- rep(NA, nrow(dat))
      return(list(delta.sim = beta.sim, theta.sim = theta.sim))
    }
  }

  #============= End fit item parameters =============================

  #============= Fit person parameters: WML/MLE ======================

  if (!is.null(method.person)) {
    #------------- Define item response function -----------------------
    arg.list <- list("b" = beta.sim)

    pcmfct0 <- function(theta, b, ii){

      if (!all(any(class(b) %in% c("matrix", "data.frame")))) {
        stop("b is not a matrix or data.frame")
      }

      beta <- b#delta2beta(delta = delta)

      N <- length(theta)  # number of persons
      M <- nrow(beta)     # max number of categories - 1 for items

      beta0 <- colMeans(beta)#0#- sum(beta[, ii]) #
      matb <- matrix(c(beta0[ii], beta[, ii]), nrow = N, ncol = M+1, byrow = TRUE)
      matx <- matrix(0:M, nrow = N, ncol = M+1, byrow = TRUE)
      eta <- exp(theta * matx + matb)
      pbs <- eta / rowSums(eta, na.rm=TRUE)
      return(pbs)
    }

    person.fit <- sirt::IRT.mle(data = dat, irffct = pcmfct0, arg.list = arg.list,
                                type = method.person, progress = FALSE)
    theta.sim <- person.fit$est
    theta.sim[theta.sim %in% c(Inf, -Inf)] <- NA
  } else {
    theta.sim <- NULL
    preson.fit <- NULL
  }


  #============= End fit person parameters ===========================

  delta.sim <- beta2delta(beta = beta.sim, x = dat)

  out <- list(delta = delta.sim, theta = theta.sim,
              item.fit = item.fit, person.fit = person.fit)
  out$call <- cl

  out
}

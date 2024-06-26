#' Gamma polynomials (recursive formula)
#' @param pars A vector of item parameters
#' @param r The total score
#' @param par.grp The grouping of scores
#'
#' @noRd
gamma_r_rec_pcm <- memoise::memoise(function(pars, r, par.grp){

  if (r == 0)
    return(1)

  if (r > length(pars) | r<0)
    return(0)

  if (r != 0 | r <= length(pars)) {

    A <- exp(pars[par.grp==1])
    B <- sapply(1:length(pars[par.grp==1]), FUN = function(x){
      gamma_r_rec_pcm(pars = pars[par.grp!=1],
                      r = r-x,
                      par.grp = par.grp[par.grp!=1]-1)})
    C <- gamma_r_rec_pcm(pars = pars[par.grp!=1], r = r, par.grp = par.grp[par.grp!=1]-1)

    return({sum(A * B) + C})

  }
})
#' Function for computing moments
#' @param x A vector of total scores
#'
#' @noRd
momfct <- function(Tot.val, par.itemgrp, betas, R, itm, error.band) {
  sapply(Tot.val, FUN = function(R) {
    l <- par.itemgrp[par.itemgrp!=itm]
    par.itemgrp_noitem <- ifelse(l > itm, l-1, l)

    #==================== call gamma polynomials (recursive formula) =========
    g1 <- gamma_r_rec_pcm(betas, R, par.itemgrp)

    EXP <- sum(sapply(1:sum(par.itemgrp==itm), FUN = function(X) {
      g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itm], R-X, par.itemgrp_noitem)
      X*exp(betas[par.itemgrp==itm][X])*g2/g1
    }))

    if (error.band) {
      VAR <- sum(sapply(1:sum(par.itemgrp==itm), FUN = function(X) {
        g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itm], R-X, par.itemgrp_noitem)
        X^2*exp(betas[par.itemgrp==itm][X])*g2/g1
      }))
    } else {
      VAR <- NULL
    }
    list(EXP = EXP, VAR = VAR)
    #=================== end call gamma polynomials ==========================
  })
}
#' A function factory for getting integer y-axis values.
#' @param n n
#' @noRd
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


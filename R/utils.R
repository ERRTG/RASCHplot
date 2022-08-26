#' Gamma polynomials (recursive formula)
#' @param pars A vector of item parameters
#' @param r The total score
#' @param par.grp The grouping of scores
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

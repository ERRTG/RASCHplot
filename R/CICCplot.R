#' This function constructs Conditional Item Characteristic Curves for selected items in a Rasch-model. These plots can be used to investigate item misfit.
#'
#' @param model A model object of class `Rm` or `eRm` returned from the functions `RM()` or `PCM()` from the `eRm` package.
#' @param which.item An integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is `which.item = 1`. The argument will not be used if `all.items = TRUE`.
#' @param lower.groups A vector for grouping the set of possible total scores into intervals, for which the empirical expected item-score will be calculated and added to the plot. The vector should contain the lower points of the intervals, that the set of possible total scores should be divided into. If zero does not appear in the vector, it will be added automatically. If `lower.groups = "all"` (default), the empirical expected item-score will be plotted for every possible total score.
#' @param all.items Logical flag for constructing CICC plots for all items in the data. Default value is `FALSE`.
#' @param error.bar Logical flag for adding errorbars illustrating the empirical confidence interval of the observed means of the conditional item score. The confidence intervals are calculated as follows: For each interval l of the total score, induced by the lower-groups argument, the mean x_l, variance var(x_l), and number of observations n_l within the interval of the total score will be calculated. The confidence interval for the mean x_l is then found as \eqn{x_l \pm 2\cdot \sqrt(\frac{var(x_l)}{n_l})}. Default value is `TRUE`.
#' @param grid.items  Logical flag for arranging the items selected by which.item or all.items in grids using the `ggarrange` function from the `ggpubr` package. Default value is `FALSE`.
#' @param ... Arguments to be passed to `ggarrange`. The argument will only be used if 'grid.items = TRUE'.
#'
#' @import ggplot2
#' @import memoise
#' @import ggpubr
#' @import scales
#' @import stats
#'
#' @return CICC plot
#'
#' @examples
#' library(iarm)
#' str(amts)
#' it.AMTS <- amts[, 4:13]
#' it.AMTS.complete <- it.AMTS[complete.cases(it.AMTS), ]
#' model.AMTS <- RM(it.AMTS.complete, sum0 = FALSE)
#' CICCplot(model = model.AMTS, lower.groups = "all")
#' p <- CICCplot(model = model.AMTS, lower.groups = "all")
#' p + scale_colour_manual(values = c("blue","orange"))
#' CICCplot(model = model.AMTS, all.items = TRUE)
#' CICCplot(model = model.AMTS, lower.groups = c(0, 1, 2,5,8,10))
#'
#' @export CICCplot
#'
CICCplot <- function(model, which.item = 1, lower.groups = "all", all.items = FALSE, grid.items = FALSE, error.bar = TRUE, ...){

  itmidx <- suppressWarnings(as.numeric(which.item))
  if(anyNA(itmidx)){
    stop("all values of which.item can not be converted to numeric")
  }

  data <- model$X
  betas <- model$betapar
  k <- ncol(data)
  m_i <- sapply(1:k, FUN = function(i) length(unique(data[,i]))-1)
  parsidx <- rep(1:k, m_i)

  n.itemcat <- apply(data, 2, FUN = function(x){max(x, na.rm = T) - min(x, na.rm = T)})
  par.itemgrp <- rep(1:ncol(data), times = n.itemcat)

  if (is.double(lower.groups) | is.integer(lower.groups)) {
    if (any(lower.groups>length(betas)))
      stop("lower.group index greater than maximum possible score")
  }
  if (any(itmidx > k))
    stop("some values of which.item are greater than number of items in the model")

  if (any(itmidx < 1)) {
    stop("some values of which.item < 1")
  }

  if (all(length(itmidx)==1 & is.double(itmidx) & !all.items)) {

    #Tot.val <- 0:length(phi)
    Tot.val <- 0:length(betas)

    exp.val <- sapply(Tot.val, FUN = function(R){
      l <- par.itemgrp[par.itemgrp!=itmidx]
      par.itemgrp_noitem <- ifelse( l > itmidx, l-1, l)
      g1 <- gamma_r_rec_pcm(betas, R, par.itemgrp)
      return( sum( sapply( 1:sum(par.itemgrp==itmidx), FUN = function(X){
        g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itmidx], R-X, par.itemgrp_noitem)
        return( X*exp(betas[par.itemgrp==itmidx][X])*g2/g1)})))
    })

    data_exp <- data.frame(Tot.val, exp.val)

    if (!is.double(lower.groups) & !is.integer(lower.groups)){
      if (lower.groups == "all"){

        #Tot.val_grp <- 0:length(phi)
        Tot.val_grp <- 0:length(betas)
        obs.val_grp <- sapply(Tot.val_grp, FUN = function(x){ mean( data[which(rowSums(data) == x), itmidx] )})
        var.val_grp <- sapply(Tot.val_grp, FUN = function(x){ var( data[which(rowSums(data) == x), itmidx] )})
        n.val_grp <- sapply(Tot.val_grp, FUN = function(x){ length( data[which(rowSums(data) == x), itmidx] )})
      }}

    if (is.double(lower.groups)|is.integer(lower.groups)){

      breaks <- sort(x = unique(c(floor(lower.groups), min(Tot.val))))
      n.groups <- length(breaks)

      Tot.val_grp <- rep(NA, times = n.groups)
      obs.val_grp <- rep(NA, times = n.groups)
      var.val_grp <- rep(NA, times = n.groups)
      n.val_grp <- rep(NA, times = n.groups)

      for (i in seq_along(breaks)){

        if(i != n.groups){

          obs.val_grp[i] <- mean( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itmidx])
          var.val_grp[i] <- var( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itmidx])
          n.val_grp[i] <- length( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itmidx])
          Tot.val_grp[i] <- mean( rowSums(data)[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1))])

        } else{

          obs.val_grp[i] <- mean( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itmidx])
          var.val_grp[i] <- var( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itmidx])
          n.val_grp[i] <- length( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itmidx])
          Tot.val_grp[i] <- mean( rowSums(data)[which(rowSums(data) %in% breaks[i]:max(Tot.val))])
        }
      }}

    data_obs <- data.frame(Tot.val_grp, obs.val_grp, var.val_grp, n.val_grp, CI.bound = NA)
    data_obs <- data_obs[data_obs$n.val_grp != 0, ]

    if(error.bar){data_obs$CI.bound <- 1.96*sqrt(data_obs[,3]/data_obs[,4])}

    col <- c("Expected" = "darkgrey", "Observed" = "orange")
    itmtit <- colnames(data)[itmidx]

    P <- ciccplot(data_exp, Tot.val, exp.val, data_obs, Tot.val_grp, obs.val_grp, itmtit, CI.bound, col)
  }

  if (all.items | length(itmidx)>1 ){

    which.item.arg <- itmidx

    if(all.items){
      n.items <- dim(data)[2]
      pp <- list(rep(NA, n.items))
      ii <- 1:n.items}

    if((!all.items) & (length(itmidx)>1) ){
      pp <- list(rep(NA, length(itmidx)))
      ii <- itmidx}

    j <- 1

    Tot.val <- 0:length(betas)

    for (k in ii) {

      itmidx <- k
      l <- par.itemgrp[par.itemgrp!=itmidx]
      par.itemgrp_noitem <- ifelse( l > itmidx, l-1, l)

      exp.val <- sapply(Tot.val, FUN = function(R){
        g1 <- gamma_r_rec_pcm(betas, R, par.itemgrp)
        return( sum( sapply( 1:sum(par.itemgrp==itmidx), FUN = function(X){
          g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itmidx], R-X, par.itemgrp_noitem)
          return( X*exp(betas[par.itemgrp==itmidx][X])*g2/g1)})))
      })
      data_exp <- data.frame(Tot.val, exp.val)

      if (!is.double(lower.groups) & !is.integer(lower.groups)){

        if (lower.groups == "all"){

          # Tot.val_grp <- 0:length(phi)
          Tot.val_grp <- 0:length(betas)
          obs.val_grp <- sapply(Tot.val_grp, FUN = function(x){ mean( data[which(rowSums(data) == x), itmidx] )})
          var.val_grp <- sapply(Tot.val_grp, FUN = function(x){ var( data[which(rowSums(data) == x), itmidx] )})
          n.val_grp <- sapply(Tot.val_grp, FUN = function(x){ length( data[which(rowSums(data) == x), itmidx] )})
        }}

      if (is.double(lower.groups)|is.integer(lower.groups)){

        breaks <- sort(x=unique(c(floor(lower.groups),min(Tot.val))))
        n.groups <- length(breaks)

        Tot.val_grp <- rep(NA, times = n.groups)
        obs.val_grp <- rep(NA, times = n.groups)
        var.val_grp <- rep(NA, times = n.groups)
        n.val_grp <- rep(NA, times = n.groups)

        for (i in seq_along(breaks)){

          if(i != n.groups){

            obs.val_grp[i] <- mean(data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itmidx])
            var.val_grp[i] <- var(data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itmidx])
            n.val_grp[i] <- length(data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itmidx])
            Tot.val_grp[i] <- mean(rowSums(data)[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1))])

          } else{

            obs.val_grp[i] <- mean(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itmidx])
            var.val_grp[i] <- var(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itmidx])
            n.val_grp[i] <- length(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itmidx])
            Tot.val_grp[i] <- mean(rowSums(data)[which(rowSums(data) %in% breaks[i]:max(Tot.val))])
          }
        }}


      data_obs <- data.frame(Tot.val_grp, obs.val_grp, var.val_grp, n.val_grp, CI.bound = NA)

      if (error.bar){ data_obs$CI.bound <- 1.96*sqrt(data_obs[,3]/data_obs[,4]) }

      col <- c("Expected" = "darkgrey", "Observed" = "orange")
      itmtit <- colnames(model$X)[itmidx]

      pp[[j]] <- ciccplot(data_exp, Tot.val, exp.val, data_obs, Tot.val_grp, obs.val_grp, itmtit, CI.bound, col)
      j <- j+1

    }

    if (grid.items){

      if (all.items){ P <- ggpubr::ggarrange(plotlist= pp, ...)}
      if (!all.items){P <- ggpubr::ggarrange(plotlist= pp, ...)}
    }
    if (!grid.items){ P <- pp}

  }
  P
}
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
#' Internal CICC plot function
#' @param data_exp data_exp
#' @param Tot.val Tot.val
#' @param exp.val exp.val
#' @param data_obs data_obs
#' @param Tot.val_grp Tot.val_grp
#' @param obs.val_grp obs.val_grp
#' @param itmtit itmtit
#' @param CI.bound CI.bound
#' @param col col
#' @noRd
ciccplot <- function(data_exp, Tot.val, exp.val, data_obs, Tot.val_grp, obs.val_grp, itmtit, CI.bound, col) {

  # A function factory for getting integer y-axis values.
  integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
      breaks <- floor(pretty(x, n, ...))
      names(breaks) <- attr(breaks, "labels")
      breaks
    }
    return(fxn)
  }

  x <- ggplot(data = data_exp, aes(x = Tot.val, y= exp.val, color = "Expected")) +
  geom_line() + #linetype = "dashed") +
  #geom_point() +
  geom_point(data = data_obs, aes(x = Tot.val_grp, y = obs.val_grp, color = "Observed"), size = 1) +
  scale_colour_manual(values = col) +
  ggtitle(paste0("Item: ", itmtit)) +
  xlab("Total Score") +
  ylab("Item-Score") +
  geom_errorbar(data = data_obs, aes(x = Tot.val_grp, y = obs.val_grp,
                                     ymin = obs.val_grp - CI.bound, ymax = obs.val_grp + CI.bound,
                                     color = "Observed"),
                width = 0) + #, size = .5) +
  scale_x_continuous(breaks = integer_breaks(), minor_breaks = Tot.val) + #breaks = Tot.val) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5),
        text = element_text(size = 8))
  print(x)
}
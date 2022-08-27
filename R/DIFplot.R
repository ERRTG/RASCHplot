#' This function constructs Conditional Item Characteristic Curves for selected items in a Rasch-model and allows for stratification on categorical variables. These plots can be used to investigate item misfit.
#'
#' @param model A model object of class `Rm` or `eRm` returned from the functions `RM()` or `PCM()` from the `eRm` package.
#' @param which.item An integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is `which.item = 1`. The argument will not be used if `all.items = TRUE`.
#' @param strat.vars List of categorical variables for stratification.
#' @param lower.groups A vector for grouping the set of possible total scores into intervals, for which the empirical expected item-score will be calculated and added to the plot. The vector should contain the lower points of the intervals, that the set of possible total scores should be divided into. If zero does not appear in the vector, it will be added automatically. If `lower.groups = "all"` (default), the empirical expected item-score will be plotted for every possible total score.
#' @param all.items Logical flag for constructing CICC plots for all items in the data. Default value is `FALSE`.
#' @param grid.items  Logical flag for arranging the items selected by which.item or all.items in grids with at most four plots per grid. Default value is `FALSE`.
#' @param error.bar Logical flag for adding errorbars illustrating the empirical confidence interval of the observed means of the conditional item score. The confidence intervals are calculated as follows: For each interval l of the total score, induced by the lower-groups argument, the mean x_l, variance var(x_l), and number of observations n_l within the interval of the total score will be calculated. The confidence interval for the mean x_l is then found as \eqn{x_l \pm 2\cdot \sqrt(\frac{var(x_l)}{n_l})}. Default value is `TRUE`.
#'
#' @import ggplot2
#' @import memoise
#' @import ggpubr
#' @import stats
#'
#' @return CICC plot
#'
#' @examples
#' library(iarm)
#' str(amts)
#' AMTS.complete <- amts[complete.cases(amts), ]
#' it.AMTS <- AMTS.complete[, 4:13]
#' model.AMTS <- RM(it.AMTS.complete, sum0 = FALSE)
#' strat.vars = list(AMTS.complete[, "sex"], AMTS.complete[, "agegrp"])
#' DIFplot(model = model.AMTS, strat.vars = strat.vars)
#'
#' @export DIFplot
#'
DIFplot <- function(model, which.item = 1, strat.vars = NULL, lower.groups = "all", all.items = FALSE, grid.items = FALSE, error.bar = TRUE, ...){

  if(is.null(strat.vars)) {
    pp <- CICCplot(model, which.item, lower.groups, all.items, grid.items, error.bar, ...)
    warning("no variables for stratification; running CICCplot")
    return(pp)
  }

  itmidx <- suppressWarnings(as.numeric(which.item))
  if(anyNA(itmidx)){
    stop("all values of which.item can not be converted to numeric")
  }

  data <- model$X
  betas <- model$betapar
  k <- ncol(data)
  N <- nrow(data)
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
  if(!all(sapply(strat.vars, length) == N)) {
    stop("lengths of stratification variables must equal number of rows in data input of RM")
  }

  if (all(length(itmidx)==1 & is.double(itmidx) & !all.items)) {

    Tot.val <- 0:length(betas)

    exp.val <- sapply(Tot.val, FUN = function(R){
      l <- par.itemgrp[par.itemgrp!=itmidx]
      par.itemgrp_noitem <- ifelse( l > itmidx, l-1, l)
      g1 <- gamma_r_rec_pcm(betas, R, par.itemgrp)
      return(sum( sapply( 1:sum(par.itemgrp==itmidx), FUN = function(X){
        g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itmidx], R-X, par.itemgrp_noitem)
        return(X*exp(betas[par.itemgrp==itmidx][X])*g2/g1)})))
    })

    data_exp <- data.frame(Tot.val, exp.val)


    P <- vector(mode = "list", length(strat.vars))

    for(l in seq_along(strat.vars)) {

      levstrat <- levels(as.factor(strat.vars[[l]]))
      nlevstrat <- length(levstrat)

      if (!is.double(lower.groups) & !is.integer(lower.groups)){
        if (lower.groups == "all"){

          Tot.val_grp <- 0:length(betas)
          obs.val_grp <- lapply(1:nlevstrat, function(j) {
            sapply(Tot.val_grp, FUN = function(x) {
              strat.data <- data[strat.vars[[l]] == levstrat[j], ]
              mean(strat.data[which(rowSums(strat.data) == x), itmidx])
              })
            })
          var.val_grp <- lapply(1:nlevstrat, function(j) {
            sapply(Tot.val_grp, FUN = function(x) {
              strat.data <- data[strat.vars[[l]] == levstrat[j], ]
              var(strat.data[which(rowSums(strat.data) == x), itmidx])
            })
          })
          n.val_grp <- lapply(1:nlevstrat, function(j) {
            sapply(Tot.val_grp, FUN = function(x) {
              strat.data <- data[strat.vars[[l]] == levstrat[j], ]
              length(strat.data[which(rowSums(strat.data) == x), itmidx])
            })
          })
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
        }
      }

      data_obs <- lapply(1:nlevstrat, function(j) {
        df <- data.frame(Tot.val_grp,
                   obs.val_grp = obs.val_grp[[j]],
                   var.val_grp = var.val_grp[[j]],
                   n.val_grp = n.val_grp[[j]],
                   CI.bound = NA,
                   strat.var = levstrat[j])
        df <- df[df$n.val_grp != 0, ]
        if(error.bar){df$CI.bound <- 1.96*sqrt(df[,"var.val_grp"]/df[,"n.val_grp"])}
        df
      })

      itmtit <- colnames(data)[itmidx]

      data_obs_long <- do.call(rbind, data_obs)


      P[[l]] <- difplot(data_exp, Tot.val, exp.val, data_obs_long, Tot.val_grp, itmtit)

    }


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

      pp[[j]] <- difplot(data_exp, Tot.val, exp.val, data_obs, Tot.val_grp, obs.val_grp, itmtit, CI.bound, col)
      j <- j+1

    }

    if (grid.items){

      if (all.items){ P <- ggpubr::ggarrange(plotlist= pp, common.legend = T, legend = "bottom", ncol = min(2, n.items), nrow = min(2 ,ceiling(n.items/2)), align = "hv")}
      if (!all.items){P <- ggpubr::ggarrange(plotlist= pp, common.legend = T, legend = "bottom", ncol = 2, nrow = min(2 ,ceiling(length(which.item.arg)/2)), align = "hv")}
    }
    if (!grid.items){ P <- pp}

  }
  P
}
#' Internal DIF plot function
#' @param data_exp data_exp
#' @param Tot.val Tot.val
#' @param exp.val exp.val
#' @param data_obs_long data_obs_long
#' @param Tot.val_grp Tot.val_grp
#' @param itmtit itmtit
#' @noRd
difplot <- function(data_exp, Tot.val, exp.val, data_obs_long, Tot.val_grp, itmtit) {

  col <- c("darkgrey", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")[1:(nlevels(as.factor(data_obs_long$strat.var)) + 1)]
  names(col) <- c("Expected", levels(as.factor(data_obs_long$strat.var)))

  x <- ggplot(data = data_exp, aes(x = Tot.val, y= exp.val)) +
    ggtitle(paste0("Item: ", itmtit)) +
    xlab("Total Score") +
    ylab("Item-Score") +
    scale_x_continuous(breaks = Tot.val) +
    geom_line(aes(color = "Expected")) +
    geom_point(data = data_obs_long, aes(x = Tot.val_grp, y = obs.val_grp, color = strat.var), size = 1) +
    geom_errorbar(data = data_obs_long, aes(x = Tot.val_grp, y = obs.val_grp,
                                            ymin = obs.val_grp - CI.bound, ymax = obs.val_grp + CI.bound,
                                            color = strat.var),
                  width = 0.1) +
    scale_colour_manual(values = col) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.title = element_text(size = 8, hjust = 0.5),
          text = element_text(size = 8)) +
    guides(colour = guide_legend(override.aes = list(shape = c(NA, rep(1, nlevels(as.factor(data_obs_long$strat.var)))))))

  print(x)
}

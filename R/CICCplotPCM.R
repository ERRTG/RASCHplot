gamma_r_rec_pcm <- function(pars, r, par.grp){
  
  if (r == 0) return( 1 )
  if (r > length(pars)| r<0) return( 0 )
  if (r != 0 | r <= length(pars)) return( sum(exp(pars[par.grp==1]) * sapply(1:length(pars[par.grp==1]), FUN = function(x){gamma_r_rec_pcm( pars = pars[par.grp!=1], r = r-x, par.grp = par.grp[par.grp!=1]-1)})) +  gamma_r_rec_pcm(pars = pars[par.grp!=1], r = r, par.grp = par.grp[par.grp!=1]-1))
}


#' Conditional Item Characteristic Curves for Rasch models
#' 
#' This function construct Conditional Item Characteristic Curves for selected items in a Rasch-model. These plots can be used to investigate item misfit. 
#'
#' @param model A model object returned from either RM() or PCM() from \code{eRm}-package
#' @param which.item An integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. 
#' The default is \code{which.item=1}. The argument will not be used if \code{all.items = TRUE}.
#' @param lower.groups  A vector used for dividing the set of possible total scores into intervals, for which the empirical expected item-score will be calculated and added to the plot. The vector should contain the lower points of the intervals, that the set of possible total scores should be divided into. If zero does not appear in the vector, it will be added automatically. If \code{lower.groups = "all"}, the empirical expected item-score will be plotted for every possible total score.
#' @param all.items Argument of type logical. If \code{TRUE} a CICC-plot will be constructed for all items in the model-object.
#' @param grid.items Argument of type logical. If \code{TRUE}, the items selected by \code{which.item} or \code{all.items} will be arranged in grids with at most four plots per grid. Default is \code{grid.items = FALSE}.
#' @param error.bar Argument of type logical. If \code{TRUE}, errorbars illustrating the empirical confidence interval of the observed means of the conditional item score will be added to the plot(s). The confidence intervals are calculated as follows: For each interval \eqn{l} of the total score, induced by the lower-groups argument, the mean \eqn{x_l}, variance \eqn{var(x_l)}, and number of observations \eqn{n_l} within the interval of the total score will be calculated. The confidence interval for the mean \eqn{x_l} is then found as \eqn{x_l \pm 2\cdot \sqrt(\frac{var(x_l)}{n_l})}. Default value is \code{error.bar = FALSE}.
#' 
#' @param plot.settings A list containing settings for the layout of the figure(s):
#' \itemize{
#'    \item \code{color}: A list containing the colors which should be used in the plot. The list must be on the form \code{list(expected = "color1", observed = "color2")}. It is possible to choose all colors supported by the \code{ggplot2} package.The default is \code{color = NULL}, which is equivalent to \code{color1} being black and \code{color2} being red.
#'    \item \code{plot.title}: Argument with the following options for specifying the title of each CICC-plot:
#' \itemize{
#'   \item Construct a title with the number of the item(s) with \code{plot.title = "itemnumber"}.
#'   \item Use the name of the item(s) as the title of each plot using \code{plot.title = "itemnane"}.
#'   \item Add user-specified titles to each plot by stating all titles in a character vector on the form \code{plot.title = c("Title1", "Title2", ...)}. This option can not be combined with \code{"itemnumber"} and \code{"itemname"}.
#'   \item To get no title(s) use \code{plot.title = c("", "", ...)}.
#' }
#' \item \code{x.axis.seq}: A vector containing the values of the breaks on the x-axis. The default is the complete sequence of all possible total scores. 
#' \item \code{y.axis.title}: A character, which should be the label on the the y-axis. If no label is given, the default is "Item Score".
#' }
#' 
#' @return A single CICC-plot or a grid of CICC-plots.
#' @export
#' 
#' @import ggplot2
#' @import ggpubr
#' @examples
#' 

# CICCplot for PCM/RM-models
CICCplot <- function(model, which.item = 1, lower.groups = NULL, all.items = F, grid.items = T, error.bar = F, plot.settings = list( color = NULL, plot.title = NULL, x.axis.seq = NULL, y.axis.title = NULL)){
  
  data <- model$X
  betas <- model$betapar
  #print(betas)
  
  n.itemcat <- apply(data, 2, FUN = function(x){max(x, na.rm = T) - min(x, na.rm = T)})
  par.itemgrp <- rep(1:ncol(data), times = n.itemcat)
  
  #phi <- NULL
  #for(x in 1:ncol(model$X)){phi <- c(phi,-cumsum(betas[par.itemgrp==x]))}
  
  if (is.double(lower.groups)|is.integer(lower.groups)) {if(any(lower.groups>length(betas))){ message("\n lower.group index greater that maximum possible score \n"); return(NA) }}
  if (any(which.item>dim(data)[2])){ message("\n which.item index greater that number of items in the model \n"); return(NA) }
  
  
  if( length(which.item)==1 & is.double(which.item)){
    
    #Tot.val <- 0:length(phi)
    Tot.val <- 0:length(betas)
    
    {l <- par.itemgrp[par.itemgrp!=which.item]
      par.itemgrp_noitem <- ifelse( l > which.item, l-1, l)
      
      x <- 0:sum(par.itemgrp==which.item)
      g <- sapply( length(betas):(-max(x)), FUN = function(X){ gamma_r_cpp(betas[par.itemgrp!=which.item], X, par.itemgrp_noitem) })
      item.coef <- c( 1, exp(betas[par.itemgrp==which.item]))
      
      exp.val <- sapply(Tot.val, FUN = function(R){
        g_index <- g[(max(Tot.val)+1-R):(max(x)+max(Tot.val)+1 -R)]
        return(sum(x*item.coef*g_index)/sum(item.coef*g_index))
      })
    }
    
    data_exp <- data.frame(Tot.val, exp.val)
    
    if (!is.double(lower.groups) & !is.integer(lower.groups)){
      if (lower.groups == "all"){
        
        #Tot.val_grp <- 0:length(phi)
        Tot.val_grp <- 0:length(betas)
        obs.val_grp <- sapply(Tot.val_grp, FUN = function(x){ mean( data[which(rowSums(data) == x), which.item] )})
        var.val_grp <- sapply(Tot.val_grp, FUN = function(x){ var( data[which(rowSums(data) == x), which.item] )})
        n.val_grp <- sapply(Tot.val_grp, FUN = function(x){ length( data[which(rowSums(data) == x), which.item] )})
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
          
          obs.val_grp[i] <- mean( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), which.item])
          var.val_grp[i] <- var( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), which.item])
          n.val_grp[i] <- length( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), which.item])
          Tot.val_grp[i] <- mean( rowSums(data)[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1))])
          
        } else{
          
          obs.val_grp[i] <- mean( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), which.item])
          var.val_grp[i] <- var( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), which.item])
          n.val_grp[i] <- length( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), which.item])
          Tot.val_grp[i] <- mean( rowSums(data)[which(rowSums(data) %in% breaks[i]:max(Tot.val))])
        }
      }}
    
    data_obs <- data.frame(Tot.val_grp, obs.val_grp, var.val_grp, n.val_grp, CI.bound = NA)
    
    if (error.bar){ data_obs$CI.bound <- 1.96*sqrt(data_obs[,3]/data_obs[,4]) } 
    
    if (is.null(plot.settings$plot.title)) {plottitle <- paste0("Item ", which.item)} else{
      if (plot.settings$plot.title[1] == "itemname") { plottitle <- paste0(colnames(model$X)[which.item]) } else if (plot.settings$plot.title[1] == "itemnumber"){
        plottitle <- paste0("Item ", which.item) } else {plottitle <- plot.settings$plot.title[1]}}
    
    if(is.null(plot.settings$y.axis.title)){yaxistitle <- paste0("Item Score")} else{ yaxistitle <- plot.settings$y.axis.title}
    
    col <- c("Expected" = "black", "Observed" = "red")
    if (!is.null(plot.settings$color)) col <- c("Expected" = plot.settings$color$expected[1], "Observed" = plot.settings$color$observed[1])
    
    xaxis.breaks <- Tot.val
    if (!is.null(plot.settings$x.axis.seq)) xaxis.breaks <- plot.settings$x.axis.seq
    
    p <- ggplot(data = data_exp, aes(x = Tot.val, y= exp.val, color = "Expected")) + 
      geom_line(linetype = "dashed") + geom_point() +
      geom_point(data = data_obs, aes(x = Tot.val_grp, y = obs.val_grp, color = "Observed"), size = 2) +
      theme(legend.title = element_blank(), plot.title = element_text(size = 15,hjust = 0.5)) + scale_colour_manual(values = col) +
      ggtitle(plottitle) + xlab("Total Score") + ylab(yaxistitle)  + 
      scale_x_continuous(breaks= xaxis.breaks) + geom_errorbar(data = data_obs, 
                                                               aes(x = Tot.val_grp, y = obs.val_grp, ymin = obs.val_grp - CI.bound, ymax = obs.val_grp + CI.bound, color = "Observed"), 
                                                               width = 0.2, size = 1)
    
    P <- p
  }
  
  if (all.items | length(which.item)>1 ){
    
    which.item.arg <- which.item
    
    if(all.items){
      n.items <- dim(data)[2]
      pp <- list(rep(NA, n.items))
      ii <- 1:n.items}
    
    if((!all.items) & (length(which.item)>1) ){ 
      pp <- list(rep(NA, length(which.item)))
      ii <- which.item}
    
    j <- 1
    
    # Tot.val <- 0:length(phi)
    Tot.val <- 0:length(betas)
    
    # g_r <- sapply(Tot.val, FUN = function(x) gamma_r_rec(betas,x))
    
    for (k in ii) {
      
      which.item <- k 
      
      {l <- par.itemgrp[par.itemgrp!=which.item]
        par.itemgrp_noitem <- ifelse( l > which.item, l-1, l)
        
        x <- 0:sum(par.itemgrp==which.item)
        g <- sapply( length(betas):(-max(x)), FUN = function(X){ gamma_r_cpp(betas[par.itemgrp!=which.item], X, par.itemgrp_noitem) })
        item.coef <- c( 1, exp(betas[par.itemgrp==which.item]))
        
        exp.val <- sapply(Tot.val, FUN = function(R){
          g_index <- g[(max(Tot.val)+1-R):(max(x)+max(Tot.val)+1 -R)]
          return(sum(x*item.coef*g_index)/sum(item.coef*g_index))
        })
      }
      
      data_exp <- data.frame(Tot.val, exp.val)
      
      if (!is.double(lower.groups) & !is.integer(lower.groups)){
        
        if (lower.groups == "all"){
          
          # Tot.val_grp <- 0:length(phi)
          Tot.val_grp <- 0:length(betas)
          obs.val_grp <- sapply(Tot.val_grp, FUN = function(x){ mean( data[which(rowSums(data) == x), which.item] )})
          var.val_grp <- sapply(Tot.val_grp, FUN = function(x){ var( data[which(rowSums(data) == x), which.item] )})
          n.val_grp <- sapply(Tot.val_grp, FUN = function(x){ length( data[which(rowSums(data) == x), which.item] )})
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
            
            obs.val_grp[i] <- mean( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), which.item])
            var.val_grp[i] <- var( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), which.item])
            n.val_grp[i] <- length( data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), which.item])
            Tot.val_grp[i] <- mean( rowSums(data)[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1))])
            
          } else{
            
            obs.val_grp[i] <- mean( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), which.item])
            var.val_grp[i] <- var( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), which.item])
            n.val_grp[i] <- length( data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), which.item])
            Tot.val_grp[i] <- mean( rowSums(data)[which(rowSums(data) %in% breaks[i]:max(Tot.val))])
          }
        }}
      
      data_obs <- data.frame(Tot.val_grp, obs.val_grp, var.val_grp, n.val_grp, CI.bound = NA)
      
      if (error.bar){ data_obs$CI.bound <- 1.96*sqrt(data_obs[,3]/data_obs[,4]) } 
      
      if(is.null(plot.settings$plot.title)) {plottitle <- paste0("Item ", which.item)} else{
      if (plot.settings$plot.title[1] == "itemname") { plottitle <- paste0(colnames(model$X)[which.item]) } else if (plot.settings$plot.title[1] == "itemnumber"){
        plottitle <- paste0("Item ", which.item) } else {plottitle <- plot.settings$plot.title[j]}}
      
      if(is.null(plot.settings$y.axis.title)){yaxistitle <- paste0("Item Score")} else{ yaxistitle <- plot.settings$y.axis.title}
      
      col <- c("Expected" = "black", "Observed" = "red")
      if (!is.null(plot.settings$color)) col <- c("Expected" = plot.settings$color$expected[1], "Observed" = plot.settings$color$observed[1])
      
      xaxis.breaks <- Tot.val
      if (!is.null(plot.settings$x.axis.seq)) xaxis.breaks <- plot.settings$x.axis.seq
      
      p <- ggplot(data = data_exp, aes(x = Tot.val, y= exp.val, color = "Expected")) + 
        geom_line(linetype = "dashed") + geom_point() +
        geom_point(data = data_obs, aes(x = Tot.val_grp, y = obs.val_grp, color = "Observed"), size = 2) +
        theme(legend.title = element_blank(), plot.title = element_text(size = 15,hjust = 0.5)) + scale_colour_manual(values = col) +
        ggtitle(plottitle) + xlab("Total Score") + ylab(yaxistitle)  + 
        scale_x_continuous(breaks=xaxis.breaks) + 
        geom_errorbar(data = data_obs, 
                      aes(x = Tot.val_grp, y = obs.val_grp, ymin = obs.val_grp - CI.bound, ymax = obs.val_grp + CI.bound, color = "Observed"), 
                      width = 0.2, size = 1)
      
      pp[[j]] <- p 
      j <- j+1
      
    }
    
    if (grid.items){ 
      
      if (all.items){ P <- ggarrange(plotlist= pp, common.legend = T, legend = "bottom", ncol = min(2, n.items), nrow = min(2 ,ceiling(n.items/2)), align = "hv")}
      if (!all.items){P <- ggarrange(plotlist= pp, common.legend = T, legend = "bottom", ncol = 2, nrow = min(2 ,ceiling(length(which.item.arg)/2)), align = "hv")}
    }
    if (!grid.items){ P <- pp}
    
  }
  P
}


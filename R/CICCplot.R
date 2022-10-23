#' Conditional Item Characteristic Curves (CICCs)
#'
#' This function constructs CICCs for selected items in a Rasch-model. These can be used to investigate item misfit.
#'
#' A CICC describes the expected outcome of an item when conditioning on the total score of all items in the model. When plotting CICCs the total score will be on the x axis and the conditional expected item score will be on the y axis. For dichotomous items the expected item score corresponds to the probability of getting item score '1' and is always zero when the total score is zero and one when the total score is the maximal possible score. Between these to points the curve will be monotone increasing.
#'
#' \code{CICCplot()} is used to construct a \code{ggplot} object, and can be followed by + to add component to the plot.
#'
#' @param model A model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()} or \code{PCM()} from the \code{eRm} package.
#' @param which.item Either an integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is \code{which.item = 1}. Or a character string \code{"all"}for constructing CICC plots for all items in the data.
#' @param lower.groups A vector for grouping the set of possible total scores into intervals, for which the empirical expected item-score will be calculated and added to the plot. The vector should contain the lower points of the intervals, that the set of possible total scores should be divided into. If zero does not appear in the vector, it will be added automatically. If \code{lower.groups = "all"} (default), the empirical expected item-score will be plotted for every possible total score.
#' @param grid.items  Logical flag for arranging the items selected by which.item in grids using the \code{ggarrange} function from the \code{ggpubr} package. Default value is \code{FALSE}.
#' @param error.bar Logical flag for adding errorbars illustrating the empirical confidence interval of the observed means of the conditional item score. The confidence intervals are calculated as follows: For each interval l of the total score, induced by the lower-groups argument, the mean x_l, variance var(x_l), and number of observations n_l within the interval of the total score will be calculated. The confidence interval for the mean x_l is then found as \eqn{x_l \pm 2\cdot \sqrt(\frac{var(x_l)}{n_l})}. Default value is \code{TRUE}.
#' @param point.size Size aesthetics for \code{geom_point()}.
#' @param line.size Size aesthetics for \code{geom_line()}.
#' @param line.type Linetype aesthetics for \code{geom_line()}.
#' @param errorbar.width Width aesthetics for \code{geom_line()}.
#' @param errorbar.size Size aesthetics for \code{geom_errorbar()}.
#' @param ... Arguments to be passed to \code{ggarrange}. The arguments will only be used if 'grid.items = TRUE'.
#'
#' @importFrom ggplot2 ggplot aes scale_x_continuous guide_legend geom_errorbar ggtitle scale_colour_manual geom_point geom_line xlab ylab
#' @importFrom rlang .data
#' @import memoise
#' @importFrom ggpubr ggarrange
#' @import scales
#' @import stats
#'
#' @return CICC plot
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#' # Plot first item
#' CICCplot(model = model.SPADI)
#' # Plot using score groups specifying lower end of grouping intervals
#' lowerg <- c(0, 1, 2, 5, 8, 10)
#' CICCplot(model = model.SPADI, lower.groups = lowerg)
#' # Plot item 3 and 4
#' CICCplot(model = model.SPADI, which.item = c(3,4))
#' # Combined CICC plot for four items and with a common legend
#' CICCplot(model = model.SPADI, which.item = c(3:6), lower.groups = lowerg,
#'          grid.items = TRUE, common.legend = TRUE, ncol = 2, nrow = 2)
#' # Change colours and x axis limits for item 3
#' ciccobj <- CICCplot(model = model.SPADI, which.item = c(3,4))
#' ciccobj$`3` +
#'   ggplot2::scale_colour_manual(values = c("burlywood","cornflowerblue")) +
#'     ggplot2::xlim(c(2,8))
#'
#' @export CICCplot
#'
CICCplot <- function(model, which.item = 1, lower.groups = "all", grid.items = FALSE, error.bar = TRUE, point.size = 1, line.size = 1, line.type = 1, errorbar.width = 0, errorbar.size = 1, ...) {

  data <- model$X
  betas <- model$betapar
  k <- ncol(data)
  m_i <- sapply(1:k, FUN = function(i) length(unique(data[,i]))-1)
  parsidx <- rep(1:k, m_i)

  if (any(which.item == "all")) {
    itmidx <- 1:k
  } else {
    itmidx <- suppressWarnings(as.numeric(which.item))
    if (anyNA(itmidx)) {
      stop("all values of which.item can not be converted to numeric")
    }
  }


  n.itemcat <- apply(data, 2, FUN = function(x) {max(x, na.rm = T) - min(x, na.rm = T)})
  par.itemgrp <- rep(1:ncol(data), times = n.itemcat)

  if (is.double(lower.groups) | is.integer(lower.groups)) {
    if (any(lower.groups>length(betas))) {
      stop("lower.group index greater than maximum possible score")
    }
  }
  if (any(itmidx > k)) {
    stop("some values of which.item are greater than number of items in the model")
  }

  if (any(itmidx < 1)) {
    stop("some values of which.item < 1")
  }


  pp <- list(rep(NA, length(itmidx)))
  ii <- itmidx
  Tot.val <- 0:length(betas)

  plotidx <- 1

  for (itm in ii) {

    Tot.val <- 0:length(betas)

    #-------------------- Expected item response -------------------------------

    exp.val <- sapply(Tot.val, FUN = function(R) {
      l <- par.itemgrp[par.itemgrp!=itm]
      par.itemgrp_noitem <- ifelse(l > itm, l-1, l)

      #==================== call gamma polynomials (recursive formula) =========
      g1 <- gamma_r_rec_pcm(betas, R, par.itemgrp)
      return(sum(sapply(1:sum(par.itemgrp==itm), FUN = function(X) {
        g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itm], R-X, par.itemgrp_noitem)
        return(X*exp(betas[par.itemgrp==itm][X])*g2/g1)})))
      #=================== end call gamma polynomials ==========================
    })

    data_exp <- data.frame(Tot.val, exp.val)

    #==================== Observed values (no grouping) ========================

    if (!is.double(lower.groups) & !is.integer(lower.groups)) {
      if (lower.groups == "all") {

        Tot.val_grp <- 0:length(betas)
        obs.val_grp <- sapply(Tot.val_grp, FUN = function(x) {mean(data[which(rowSums(data) == x), itm])})
        var.val_grp <- sapply(Tot.val_grp, FUN = function(x) {var(data[which(rowSums(data) == x), itm])})
        n.val_grp <- sapply(Tot.val_grp, FUN = function(x) {length(data[which(rowSums(data) == x), itm])})
      }
    }

    #========================= End no grouping =================================

    #==================== Observed values (grouping) ===========================

    if (is.double(lower.groups)|is.integer(lower.groups)) {

      breaks <- sort(x = unique(c(floor(lower.groups), min(Tot.val))))
      n.groups <- length(breaks)

      Tot.val_grp <- rep(NA, times = n.groups)
      obs.val_grp <- rep(NA, times = n.groups)
      var.val_grp <- rep(NA, times = n.groups)
      n.val_grp <- rep(NA, times = n.groups)

      for (i in seq_along(breaks)) {

        if (i != n.groups) {

          obs.val_grp[i] <- mean(data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itm])
          var.val_grp[i] <- var(data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itm])
          n.val_grp[i] <- length(data[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1)), itm])
          Tot.val_grp[i] <- mean(rowSums(data)[which(rowSums(data) %in% breaks[i]:(breaks[i+1]-1))])

        } else{

          obs.val_grp[i] <- mean(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itm])
          var.val_grp[i] <- var(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itm])
          n.val_grp[i] <- length(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itm])
          Tot.val_grp[i] <- mean(rowSums(data)[which(rowSums(data) %in% breaks[i]:max(Tot.val))])
        }
      }
    }

    #========================= End grouping ====================================

    data_obs <- data.frame(Tot.val_grp, obs.val_grp, var.val_grp, n.val_grp, CI.bound = NA)
    data_obs <- data_obs[data_obs$n.val_grp != 0, ] #remove groups with no observations

    if (error.bar) {
      data_obs$CI.bound <- 1.96*sqrt(data_obs[,3]/data_obs[,4])
    }

    col <- c("Expected" = "darkgrey", "Observed" = "orange")
    itmtit <- colnames(data)[itm]

    datalist <- list(data_exp, data_obs)
    df <- bind_rows(datalist, .id="data_frame")



      pp[[plotidx]] <- ciccplot(df, itmtit, col, point.size, line.size, line.type, errorbar.width, errorbar.size, ...)


    ##ciccplot(data_exp, Tot.val, exp.val, data_obs, itmtit, col, point.size, line.size, line.type, errorbar.width, errorbar.size, ...)



    plotidx <- plotidx+1

  }

    if (grid.items) {
      P <- ggpubr::ggarrange(plotlist = pp, ...)
    } else {
      P <- pp
      names(P) <- itmidx
    }

  withCallingHandlers({
    print(P)
  }, warning=function(w) {
    if (any( grepl( "containing missing values", w)))
      invokeRestart("muffleWarning")
  })

}
#' Internal CICC plot function
#' @param df data to ggplot
#' @param itmtit itmtit
#' @param col col
#' @param point.size Size aesthetics for \code{geom_point()}.
#' @param line.size Size aesthetics for \code{geom_line()}.
#' @param line.type Linetype aesthetics for \code{geom_line()}.
#' @param errorbar.width Width aesthetics for \code{geom_line()}.
#' @param errorbar.size Size aesthetics for \code{geom_errorbar()}.
#' @noRd
ciccplot <- function(df, itmtit, col, point.size, line.size, line.type, errorbar.width, errorbar.size, ...) {

  x <- ggplot(data = df, aes(x = Tot.val, y= exp.val, color = "Expected")) +
    geom_line(size = line.size, linetype = line.type, ...) +
    geom_point(aes(x = Tot.val_grp,
                   y = obs.val_grp,
                   color = "Observed"),
               shape = 19,
               size = point.size, ...) +
    scale_colour_manual(values = col) +
    ggtitle(paste0("Item: ", itmtit)) +
    xlab("Total Score") +
    ylab("Item-Score") +
    geom_errorbar(aes(x = Tot.val_grp, y = obs.val_grp,
                      ymin = obs.val_grp - CI.bound,
                      ymax = obs.val_grp + CI.bound,
                      color = "Observed"),
                  width = errorbar.width, size = errorbar.size) +
    scale_x_continuous(breaks = integer_breaks(), minor_breaks = df$Tot.val) +
    guides(colour = guide_legend(override.aes = list(shape = c(NA, 19))))

}

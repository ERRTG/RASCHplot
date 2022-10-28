#' Stratified Conditional Item Characteristic Curves
#'
#' This function constructs Conditional Item Characteristic Curves for selected items in a Rasch-model and allows for stratification on categorical variables. These plots can be used to investigate item misfit and differential item functioning (DIF).
#'
#' ...
#'
#' @param model A model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()} or \code{PCM()} from the \code{eRm} package.
#' @param which.item Either an integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is \code{which.item = 1}. Or a character string \code{"all"} for constructing CICC plots for all items in the data.
#' @param strat.vars A named list of categorical variables for stratification.
#' @param lower.groups A named list of length \code{length(strat.vars)} of lists, vectors or a single vector for grouping the set of possible total scores into intervals, for which the empirical expected item-score will be calculated and added to the plot. The vector(s) should contain the lower points of the intervals, that the set of possible total scores should be divided into. If zero does not appear in the vector(s), it will be added automatically. If \code{lower.groups = "all"} (default), the empirical expected item-score will be plotted for every possible total score. If a list is provided, the arguments should be named corresponding to the \code{strat.vars}. If the arguments are lists, they should be named corresponding to the levels of the \code{strat.vars}.
#' @param grid.items  Logical flag for arranging the items selected by which.item in grids using the \code{ggarrange} function from the \code{ggpubr} package. Default value is \code{FALSE}.
#' @param error.bar Logical flag for adding errorbars illustrating the empirical confidence interval of the observed means of the conditional item score. The confidence intervals are calculated as follows: For each interval l of the total score, induced by the lower-groups argument, the mean x_l, variance var(x_l), and number of observations n_l within the interval of the total score will be calculated. The confidence interval for the mean x_l is then found as \eqn{x_l \pm 2\cdot \sqrt(\frac{var(x_l)}{n_l})}. Default value is \code{TRUE}.
#' @param dodge.width Dodging width of error bars. To prevent overlapping error bars, dodging (jittering) preserves the vertical position of error bars while adjusting the horizontal position. Default is \code{dodge.width = 0.5}.
#' @param point.size Size aesthetics for \code{geom_point()}.
#' @param line.size Size aesthetics for \code{geom_line()}.
#' @param line.type Linetype aesthetics for \code{geom_line()}.
#' @param errorbar.width Width aesthetics for \code{geom_line()}.
#' @param errorbar.size Size aesthetics for \code{geom_errorbar()}.
#' @param ... Arguments to be passed to \code{ggarrange}. The arguments will only be used if \code{grid.items = TRUE}.
#'
#' @importFrom ggplot2 ggplot aes scale_x_continuous guide_legend geom_errorbar ggtitle scale_colour_manual geom_point geom_line xlab ylab position_dodge
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @import memoise
#' @import ggpubr
#' @import stats
#'
#' @return CICC plot
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' model.SPADI <- eRm::PCM(it.SPADI)
#' strat.vars <- list(gender = SPADI.complete[, "gender"])
#' DIFplot(model = model.SPADI, strat.vars = strat.vars)
#' strat.vars <- list(gender = SPADI.complete[, "gender"], over60 = SPADI.complete[, "over60"])
#' DIFplot(model = model.SPADI, strat.vars = strat.vars)
#' DIFplot(model = model.SPADI, which.item = c(1,2), strat.vars = strat.vars)
#' lower.groups <- list(gender = c(0, 1, 2, 5, 8, 10), over60 = c(0, 3, 6, 9))
#' DIFplot(model = model.SPADI, strat.vars = strat.vars, lower.groups = lower.groups)
#' DIFplot(model = model.SPADI, which.item = c(1,2),
#'         strat.vars = strat.vars, lower.groups = lower.groups)
#' lower.groups <- list(gender = list("1" = c(0, 5, 15, 20, 30, 40),
#'                                    "2" = c(0,  5, 10)),
#'                      over60 = list("0" = c(0, 10, 15, 20),
#'                                    "1" = c(0, 8, 15, 17, 18, 22, 29, 38)))
#' DIFplot(model = model.SPADI, strat.vars = strat.vars, lower.groups = lower.groups)
#'
#' @export DIFplot
#'
DIFplot <- function(model, which.item = 1, strat.vars, lower.groups = "all", grid.items = FALSE, error.bar = TRUE, dodge.width = 0.5, point.size= 1, line.size = 1, line.type = 1, errorbar.width = 0.1, errorbar.size = 1, ...) {

  if (missing(strat.vars)) {
    pp <- CICCplot(model, which.item, lower.groups, grid.items, error.bar, point.size, line.size, line.type, errorbar.width, errorbar.size, ...)
    warning("no variables for stratification; running CICCplot")
    return(pp)
  }

  data <- model$X
  betas <- model$betapar
  k <- ncol(data)
  N <- nrow(data)
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

  if (is.null(names(strat.vars))) {
    stop("the list of stratification variables is not named")
  }

  if (is.list(lower.groups)) {
    if (is.null(names(lower.groups))) {
      stop("the list lower.groups is not named")
    }
    if (!all(names(lower.groups) == names(strat.vars))) {
      stop("names of lower.groups and strat.vars must be identical")
    }
    if (all(sapply(lower.groups, is.list))) {
      if (!all(sapply(lower.groups, length) == sapply(strat.vars, function(x) nlevels(as.factor(x))))) {
        stop("length of lists in lower.groups list must be equal to number of levels in corresponding strat.vars")
      }
    }
  }

  n.itemcat <- apply(data, 2, FUN = function(x) {
    max(x, na.rm = T) - min(x, na.rm = T)
    }
  )
  par.itemgrp <- rep(1:ncol(data), times = n.itemcat)

  if (all(sapply(lower.groups, is.double)) | all(sapply(lower.groups, is.integer))) {
      if (any(unlist(lower.groups) > length(betas)))
        stop("lower.group index greater than maximum possible score")
  }
  if (any(itmidx > k)) {
    stop("some values of which.item are greater than number of items in the model")
  }
  if (any(itmidx < 1)) {
    stop("some values of which.item < 1")
  }
  if (!all(sapply(strat.vars, length) == N)) {
    stop("lengths of stratification variables must equal number of rows in data input of RM")
  }

  pp <- list(rep(NA, length(itmidx)))
  ii <- itmidx
  Tot.val <- 0:length(betas)

  plotidx <- 1

  for (itm in ii) {

    exp.val <- sapply(Tot.val, FUN = function(R) {
      l <- par.itemgrp[par.itemgrp!=itm]
      par.itemgrp_noitem <- ifelse(l > itm, l-1, l)
      g1 <- gamma_r_rec_pcm(betas, R, par.itemgrp)
      return(sum(sapply(1:sum(par.itemgrp==itm), FUN = function(X) {
        g2 <- gamma_r_rec_pcm(betas[par.itemgrp!=itm], R-X, par.itemgrp_noitem)
        return(X*exp(betas[par.itemgrp==itm][X])*g2/g1)
        })
      ))
    })

    data_exp <- data.frame(Tot.val, exp.val)

    pp[[plotidx]] <- vector(mode = "list", length(strat.vars))
    names(pp[[plotidx]]) <- names(strat.vars)

    for(l in seq_along(strat.vars)) {

      stratname <- names(strat.vars)[l]
      levstrat <- levels(as.factor(strat.vars[[l]]))
      nlevstrat <- length(levstrat)

      if (!is.double(lower.groups) & !is.integer(lower.groups)) {
        if (all(lower.groups == "all")) {

          Tot.val_grp <- 0:length(betas)
          obs.val_grp <- lapply(1:nlevstrat, function(j) {
            sapply(Tot.val_grp, FUN = function(x) {
              strat.data <- data[strat.vars[[l]] == levstrat[j], ]
              mean(strat.data[which(rowSums(strat.data) == x), itm])
             })
          })
          var.val_grp <- lapply(1:nlevstrat, function(j) {
            sapply(Tot.val_grp, FUN = function(x) {
              strat.data <- data[strat.vars[[l]] == levstrat[j], ]
              var(strat.data[which(rowSums(strat.data) == x), itm])
            })
          })
          n.val_grp <- lapply(1:nlevstrat, function(j) {
            sapply(Tot.val_grp, FUN = function(x) {
              strat.data <- data[strat.vars[[l]] == levstrat[j], ]
              length(strat.data[which(rowSums(strat.data) == x), itm])
            })
          })
          Tot.val_grp <- lapply(1:nlevstrat, function(j) 0:length(betas))
        }
      }

      if (is.double(unlist(lower.groups))|is.integer(unlist(lower.groups))) {

        if (is.list(lower.groups)) {
          lgrpsgrps <- lower.groups[[stratname]]
        } else {
          lgrpsgrps <- lower.groups
        }
        if(!is.list(lgrpsgrps)) {
          lgrpsgrps <- lapply(1:nlevstrat, function(x) lgrpsgrps) #list(lgrpsgrps)
          names(lgrpsgrps) <- levstrat
        }
        #if(!all(sapply(lgrpsgrps, is.list))) {
        #  lgrpsgrps <- lapply(1:nlevstrat, function(x) lgrpsgrps)
        #}

        Tot.val_grp <- vector(mode = "list", length(lgrpsgrps))
        obs.val_grp <- vector(mode = "list", length(lgrpsgrps))
        var.val_grp <- vector(mode = "list", length(lgrpsgrps))
        n.val_grp <- vector(mode = "list", length(lgrpsgrps))

        for (j in seq_along(lgrpsgrps)) { #1:nlevstrat) {

          strat.data <- data[strat.vars[[l]] == levstrat[j], ]

          #for(lgrpsidx in seq_along(lgrpsgrps)) {

            lgrps <- lgrpsgrps[[levstrat[j]]] # unlist(lgrpsgrps[[j]])

            breaks <- sort(x = unique(c(floor(lgrps), min(Tot.val))))
            n.groups <- length(breaks)

            for (i in seq_along(breaks)) {

              if (i != n.groups) {

                obs.val_grp[[j]][i] <- mean(strat.data[which(rowSums(strat.data) %in% breaks[i]:(breaks[i+1]-1)), itm])
                var.val_grp[[j]][i] <- var(strat.data[which(rowSums(strat.data) %in% breaks[i]:(breaks[i+1]-1)), itm])
                n.val_grp[[j]][i] <- length(strat.data[which(rowSums(strat.data) %in% breaks[i]:(breaks[i+1]-1)), itm])
                Tot.val_grp[[j]][i] <- mean(rowSums(strat.data)[which(rowSums(strat.data) %in% breaks[i]:(breaks[i+1]-1))])

              } else{

                obs.val_grp[[j]][i] <- mean(strat.data[which(rowSums(strat.data) %in% breaks[i]:max(Tot.val)), itm])
                var.val_grp[[j]][i] <- var(strat.data[which(rowSums(strat.data) %in% breaks[i]:max(Tot.val)), itm])
                n.val_grp[[j]][i] <- length(strat.data[which(rowSums(strat.data) %in% breaks[i]:max(Tot.val)), itm])
                Tot.val_grp[[j]][i] <- mean(rowSums(strat.data)[which(rowSums(strat.data) %in% breaks[i]:max(Tot.val))])
              }
            }

          #}

        }

      }

      data_obs <- lapply(1:nlevstrat, function(j) {
        df <- data.frame(Tot.val_grp = Tot.val_grp[[j]],
                         obs.val_grp = obs.val_grp[[j]],
                         var.val_grp = var.val_grp[[j]],
                         n.val_grp = n.val_grp[[j]],
                         CI.bound = NA,
                         strat.var = levstrat[j])
        df <- df[df$n.val_grp != 0, ]
        if (error.bar) {
          df$CI.bound <- 1.96*sqrt(df[,"var.val_grp"]/df[,"n.val_grp"])
        }
        df
      })

      itmtit <- colnames(data)[itm]

      data_obs_long <- do.call(rbind, data_obs)
      cidx <- 1:(nlevels(as.factor(data_obs_long$strat.var)) + 1)
      col <- c("darkgrey", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")[cidx]
      names(col) <- c("Expected", levels(as.factor(data_obs_long$strat.var)))

      datalist <- list(data_exp, data_obs_long)
      df <- bind_rows(datalist, .id="data_frame")

      pp[[plotidx]][[l]] <- difplot(df, itmtit, stratname, col, dodge.width, point.size, line.size, line.type, errorbar.width, errorbar.size, ...)

    }

    plotidx <- plotidx + 1
}

    if (grid.items) {

       P <- ggpubr::ggarrange(plotlist = pp, ...)

    }
    if (!grid.items) P <- pp

  P
}
#' Internal DIF plot function
#' @param df data to ggplot
#' @param itmtit itmtit
#' @param stratname stratname
#' @param col col
#' @param dodge.width dodge.width
#' @param point.size Size aesthetics for \code{geom_point()}.
#' @param line.size Size aesthetics for \code{geom_line()}.
#' @param line.type Linetype aesthetics for \code{geom_line()}.
#' @param errorbar.width Width aesthetics for \code{geom_line()}.
#' @param errorbar.size Size aesthetics for \code{geom_errorbar()}.
#' @noRd
difplot <- function(df, itmtit, stratname, col, dodge.width, point.size, line.size, line.type, errorbar.width, errorbar.size, ...) {

  x <- ggplot(data = df, aes(x = .data$Tot.val, y= .data$exp.val)) +
    ggtitle(paste0("Item: ", itmtit)) +
    xlab("Total Score") +
    ylab("Item-Score") +
    scale_x_continuous(breaks = integer_breaks(), minor_breaks = df$Tot.val) +
    geom_line(aes(color = "Expected"), size = line.size, linetype = line.type, ...) +
    geom_point(aes(x = .data$Tot.val_grp,
                   y = .data$obs.val_grp,
                   color = .data$strat.var),
               position = position_dodge(width = dodge.width),
               size = point.size, ...) +
    geom_errorbar(aes(x = .data$Tot.val_grp,
                      y = .data$obs.val_grp,
                      ymin = .data$obs.val_grp - .data$CI.bound,
                      ymax = .data$obs.val_grp + .data$CI.bound,
                      color = .data$strat.var),
                  width = errorbar.width, size = errorbar.size,
                  position = position_dodge(width = dodge.width)) +
    scale_colour_manual(values = col) +
    guides(colour = guide_legend(title = stratname,
                                 override.aes = list(shape = c(NA, rep(19, nlevels(as.factor(df$strat.var)))))))
  #+ guides(colour = guide_legend(stratname)) #
  #+ #+ labs(fill = stratname)
  #+ #+ theme(legend.title=element_blank()) #scale_color_discrete(name = "")
}

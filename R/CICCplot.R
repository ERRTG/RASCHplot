#' Conditional Item Characteristic Curves (CICCs)
#'
#' This function constructs CICCs for selected items in a Rasch-model. These can be used to investigate item misfit.
#'
#' A CICC describes the expected outcome of an item when conditioning on the total score of all items in the model. When plotting CICCs the total score will be on the x axis and the conditional expected item score will be on the y axis. For dichotomous items the expected item score corresponds to the probability of getting item score '1' and is always zero when the total score is zero and one when the total score is the maximal possible score. Between these to points the curve will be monotone increasing.
#'
#' \code{CICCplot()} is used to construct a \code{ggplot} object, and can be followed by + to add component to the plot.
#'
#' @param model A model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()}, \code{PCM()}, or \code{RSM()} from the \code{eRm} package.
#' @param which.item An indexing vector specifying the item(s) for which a CICC-plot should be constructed. Either an integer or vector of integers to select items by position, or a character string or a vector of character strings to select items by their names. Or a character string \code{"all"} for constructing CICC plots for all items in the data. The default is \code{which.item = 1}.
#' @param lower.groups A vector for grouping the set of possible total scores into intervals, for which the empirical expected item-score will be calculated and added to the plot. The vector should contain the lower points of the intervals, that the set of possible total scores should be divided into. If zero does not appear in the vector, it will be added automatically. If \code{lower.groups = "all"} (default), the empirical expected item-score will be plotted for every possible total score.
#' @param grid.items  Logical flag for arranging the items selected by which.item in grids using the \code{ggarrange} function from the \code{ggpubr} package. Default value is \code{FALSE}.
#' @param observed Logical flag for adding observed average item scores. Default value is \code{TRUE}.
#' @param error.bar Logical flag for adding error bars illustrating the empirical confidence interval of the observed means of the conditional item score. The confidence intervals are calculated as follows: For each interval l of the total score, induced by the lower-groups argument, the mean x_l, variance var(x_l), and number of observations n_l within the interval of the total score will be calculated. The confidence interval for the mean x_l is then found as \eqn{x_l \pm 2\cdot \sqrt(\frac{var(x_l)}{n_l})}. Default value is \code{TRUE}.
#' @param error.level The confidence level required. Default is a 95% confidence level.
#' @param point.size Size aesthetics for \code{geom_point()}.
#' @param line.size Size aesthetics for \code{geom_line()}.
#' @param line.type Linetype aesthetics for \code{geom_line()}.
#' @param errorbar.width Width aesthetics for \code{geom_line()}.
#' @param errorbar.size Size aesthetics for \code{geom_errorbar()}.
#' @param lower.group.bg Altering lower group background colour. Default is rgb(.6,.6,.6) and rgb(.8,.8,.8).
#' @param legend.title Legend title. Default is empty.
#' @param lower.group.col Colour of observed points.
#' @param error.band Default is FALSE.
#' @param error.band.col Colour of error band (if \code{error.band = TRUE}).
#' @param smooth.error.band Logical flag.
#' @param ... Arguments to be passed to \code{ggarrange}. The arguments will only be used if 'grid.items = TRUE'.
#'
#' @rawNamespace import(stats, except = filter)
#' @importFrom ggplot2 ggplot aes scale_x_continuous guide_legend geom_errorbar ggtitle scale_colour_manual geom_point geom_line geom_rect xlab ylab theme_bw scale_fill_manual
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @import memoise
#' @importFrom ggpubr ggarrange
#' @importFrom grDevices rgb
#' @import scales
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
#' lowerg <- c(0, 10, 20, 30)
#' CICCplot(model = model.SPADI, lower.groups = lowerg)
#' # Adding color
#' lower.group.col <- 1:4
#' CICCplot(model = model.SPADI, lower.groups = lowerg, lower.group.col = lower.group.col)
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
#' lower.group.col <- c(rep("red", 21), rep("blue", 20))
#' CICCplot(model = model.SPADI, lower.group.col = lower.group.col)
#' # Plot first item with theoretical confidence band
#' CICCplot(model = model.SPADI, error.band = TRUE)
#' # hist(rowSums(it.SPADI))
#' # Add smmooth theoretical confidence bands
#' CICCplot(model = model.SPADI, error.band = TRUE, smooth.error.band = TRUE)
#'
#' @export CICCplot
#'
CICCplot <- function(model, which.item = 1, lower.groups = "all",
                     grid.items = FALSE, observed = TRUE, error.bar = TRUE,
                     error.level = 0.95,
                     point.size = 1, line.size = 1, line.type = 1,
                     errorbar.width = 0, errorbar.size = 1,
                     lower.group.bg = NULL, legend.title = "", lower.group.col,
                     error.band = FALSE, error.band.col = "gray",
                     smooth.error.band = FALSE, ...) {

  if (!inherits(model, c("Rm", "eRm"))) {
    stop("Object must be of class Rm or eRm")
  }


  data <- model$X
  itmnames <- colnames(data)
  betas <- model$betapar
  k <- ncol(data)
  m_i <- sapply(1:k, FUN = function(i) length(unique(data[,i]))-1)
  parsidx <- rep(1:k, m_i)

  if (any(which.item == "all")) {
    itmidx <- 1:k
  } else if (is.character(which.item)) {
    if (any(which.item %in% itmnames)) {
      itmidx <- which(itmnames %in% which.item)
    } else {
      itmidx <- suppressWarnings(as.numeric(which.item))
      if (anyNA(itmidx)) {
        stop(paste0("no items in data named ", paste(which.item, collapse = " or ")))
      } else {
        warning("all values of which.item are converted to numeric")
      }
    }
  } else {
    itmidx <- suppressWarnings(as.numeric(which.item))
    if (anyNA(itmidx)) {
      stop("all values of which.item can not be converted to numeric")
    }
  }

  if (grid.items & length(itmidx) <= 1) {
    warning("option 'grid.items' works only if CICC-plot should be constructed
            for more than one item, so the argument is ignored")
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


  pp <- vector(mode = "list", length = length(itmidx))
  ii <- itmidx
  Tot.val <- 0:length(betas)

  plotidx <- 1

  for (itm in ii) {


    #-------------------- Expected item response -------------------------------
    Tot.val <- 0:length(betas)

    exp.val.both <- sapply(Tot.val, FUN = function(R) {
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

    exp.val <- unlist(exp.val.both["EXP",])
    data_exp <- data.frame(Tot.val, exp.val)

    if (error.band) {

      z <- qnorm(error.level+(1-error.level)/2)

      n.vals <- sapply(0:length(betas), FUN = function(x) {
        length(data[which(rowSums(data) == x), itm])
      })

      data_exp$LCI <- data_exp$exp.val - z * sqrt(unlist(exp.val.both["VAR",])/n.vals)
      data_exp$UCI <- data_exp$exp.val + z * sqrt(unlist(exp.val.both["VAR",])/n.vals)

      if(smooth.error.band) {

        data_exp <- do.call(data.frame,lapply(data_exp, function(value) {
          replace(value, is.infinite(value),NA)}))
        dat <- data.frame(yL = data_exp$LCI, yU = data_exp$UCI, x = data_exp$Tot.val)
        mL <- mgcv::gam(yL ~ s(x), data = dat)
        mU <- mgcv::gam(yU ~ s(x), data = dat)

        # define finer grid of predictor values
        xnew <- data_exp$Tot.val

        # apply predict() function to the fitted GAM model
        # using the finer grid of x values
        pL <- predict(mL, newdata = data.frame(x = xnew), se = TRUE)
        pU <- predict(mU, newdata = data.frame(x = xnew), se = TRUE)

        data_exp$LCIsmooth <- pL$fit
        data_exp$UCIsmooth <- pU$fit
      }
    }


    #==================== Observed values (no grouping) ========================

    if (!is.double(lower.groups) & !is.integer(lower.groups)) {
      if (lower.groups == "all") {

        Tot.val_grp <- 0:length(betas)
        obs.val_grp <- sapply(Tot.val_grp, FUN = function(x) {mean(data[which(rowSums(data) == x), itm])})
        var.val_grp <- sapply(Tot.val_grp, FUN = function(x) {var(data[which(rowSums(data) == x), itm])})
        n.val_grp <- sapply(Tot.val_grp, FUN = function(x) {length(data[which(rowSums(data) == x), itm])})

        rects <- data.frame(xstart = NA, xend = NA, bg = NA)
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

        } else {

          obs.val_grp[i] <- mean(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itm])
          var.val_grp[i] <- var(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itm])
          n.val_grp[i] <- length(data[which(rowSums(data) %in% breaks[i]:max(Tot.val)), itm])
          Tot.val_grp[i] <- mean(rowSums(data)[which(rowSums(data) %in% breaks[i]:max(Tot.val))])
        }
      }

      rects <- data.frame(xstart = breaks,
                          xend = c(breaks[-1], max(Tot.val)),
                          bg = rep(c("1", "2"),
                                    ceiling(length(breaks)/2))[seq_along(breaks)])
    }

    #========================= End grouping ====================================

    data_obs <- data.frame(Tot.val_grp, obs.val_grp, var.val_grp, n.val_grp, CI.bound = NA)
    data_obs <- data_obs[data_obs$n.val_grp != 0, ] #remove groups with no observations

    if (error.bar) {
      z <- qnorm(error.level+(1-error.level)/2)
      data_obs$CI.bound <- z*sqrt(data_obs[,3]/data_obs[,4])
    }

    col <- c("Expected" = "darkgrey", "Observed" = "orange")

    itmtit <- colnames(data)[itm]

    datalist <- list(data_exp, data_obs, rects)
    df <- dplyr::bind_rows(datalist, .id="data_frame")

    pp[[plotidx]] <- ciccplot(df, itmtit, col, observed,
                              point.size,
                              line.size, line.type,
                              errorbar.width, errorbar.size,
                              lower.group.bg, legend.title, lower.group.col,
                              error.band, error.band.col, smooth.error.band, ...)

    plotidx <- plotidx+1

  }

  if (grid.items) {
    P <- ggpubr::ggarrange(plotlist = pp, ...)
  } else {
    P <- pp
    names(P) <- itmnames[itmidx]
  }

  P

}
#' Internal CICC plot function
#' @param df data to ggplot
#' @param itmtit itmtit
#' @param col col
#' @param observed Observed average item scores.
#' @param point.size Size aesthetics for \code{geom_point()}.
#' @param line.size Size aesthetics for \code{geom_line()}.
#' @param line.type Linetype aesthetics for \code{geom_line()}.
#' @param errorbar.width Width aesthetics for \code{geom_line()}.
#' @param errorbar.size Size aesthetics for \code{geom_errorbar()}.
#' @param lower.group.bg Background colours
#' @param legend.title Legend title.
#' @param lower.group.col Color
#' @param error.band Logical flag for plotting theoretical CI
#' @param error.band.col Color of \code{error.band}
#' @param smooth.error.band Logical flag.
#' @param ... optional parameters to be passed on to ggplot
#' @noRd
ciccplot <- function(df, itmtit, col, observed, point.size, line.size,
                     line.type, errorbar.width, errorbar.size, lower.group.bg,
                     legend.title, lower.group.col,
                     error.band, error.band.col,
                     smooth.error.band, ...) {

  if (!missing(lower.group.col)) {
    df$lgrp <- factor(df$Tot.val_grp)
    levels(df$lgrp) <- lower.group.col
  }

  x <- ggplot(data = df, aes(x = .data$Tot.val, y= .data$exp.val, color = "Expected")) +
    geom_line(linewidth = line.size, linetype = line.type, na.rm=TRUE)+#, ...) +
    xlab("Total Score") +
    ylab("Item-Score")  +
    ggtitle(paste0("Item: ", itmtit))

  if (error.band) {

    if(smooth.error.band) {
      x <- x +
        geom_ribbon(aes(ymin = .data$LCIsmooth,
                        ymax = .data$UCIsmooth),
                    fill = error.band.col, alpha = 0.5) +
        geom_line(linewidth = line.size, linetype = line.type, na.rm=TRUE)
    } else {
      x <- x +
        geom_ribbon(aes(ymin = .data$LCI,
                        ymax = .data$UCI),
                    fill = error.band.col, alpha = 0.5) +
        geom_line(linewidth = line.size, linetype = line.type, na.rm=TRUE)
    }

  }


  if (!all(is.na(df$bg))) {

    if (is.null(lower.group.bg)) {
      lower.group.bg <- c(rgb(.6,.6,.6), rgb(.8,.8,.8))
    }

    x <- x +
      geom_rect(aes(ymin = 0, ymax = max(.data$exp.val, na.rm = TRUE),
                    xmin = .data$xstart, xmax = .data$xend, fill = .data$bg),
                alpha = 0.2, inherit.aes = FALSE, na.rm=TRUE) +
      theme_bw() + theme(panel.border = element_blank()) +
      guides(fill = "none") +
      scale_fill_manual(values = lower.group.bg) +
    scale_x_continuous(breaks = integer_breaks(), minor_breaks = df$Tot.val) +
      guides(colour = guide_legend(override.aes = list(shape = c(NA, 19)))) +
      guides(colour = guide_legend(title = legend.title))

  }


  if (observed) {
    if (missing(lower.group.col)) {
      x <- x +
        geom_point(aes(x = .data$Tot.val_grp,
                       y = .data$obs.val_grp,
                       color = "Observed"),
                   shape = 19,
                   size = point.size, na.rm=TRUE, ...) +
        scale_colour_manual(values = col) +
        geom_errorbar(aes(x = .data$Tot.val_grp, y = .data$obs.val_grp,
                          ymin = .data$obs.val_grp - .data$CI.bound,
                          ymax = .data$obs.val_grp + .data$CI.bound,
                          color = "Observed"),
                      width = errorbar.width, linewidth = errorbar.size) +
        guides(colour = guide_legend(override.aes = list(shape = c(NA, 19)))) +
        guides(colour = guide_legend(title = legend.title))
    } else {
      col <- c("darkgrey", levels(factor(lower.group.col)))
      names(col) <- c("Expected", levels(factor(lower.group.col)))
      lbls <- c("Expected", rep("Observed", nlevels(factor(lower.group.col))))
      x <- x +
        geom_point(aes(x = .data$Tot.val_grp,
                       y = .data$obs.val_grp,
                       color = .data$lgrp),
                   shape = 19,
                   size = point.size, na.rm=TRUE)+#, ...) +
      scale_colour_manual(values = col, limits = names(col), labels = lbls) +
      geom_errorbar(aes(x = .data$Tot.val_grp, y = .data$obs.val_grp,
                        ymin = .data$obs.val_grp - .data$CI.bound,
                        ymax = .data$obs.val_grp + .data$CI.bound,
                        color = .data$lgrp),
                    width = errorbar.width, linewidth = errorbar.size) +
        guides(colour = guide_legend(override.aes = list(shape = c(NA, 19), color = c(col[1], NA)))) +
        guides(colour = guide_legend(title = legend.title))
    }
  }

  x

}

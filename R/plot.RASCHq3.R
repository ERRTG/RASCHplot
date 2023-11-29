#' Density plot function
#'
#' Visualise the empirical distribution of Yen's Q3 for a \code{RASCHq3} object.
#'
#' @param x Vector of statistics to be plotted.
#' @param extreme Values to be plotted
#' @param probs Vector of percentiles.
#' @param breaks Vector of breaks.
#' @param labels Vector of labels
#' @param colours Vector of colours
#' @param xtitle Character string for x axis title
#' @param title Character string for plot title
#' @param col.outline Colour of outline of density
#' @param alpha.ribbon Transparency (alpha) of density.
#' @param title.legend Legend title.
#' @param ... Other paramters to be passed through to plotting functions.
#'
#' @importFrom stats density quantile runif
#' @importFrom rlang .data
#' @importFrom data.table rleid
#' @importFrom zoo na.locf
#' @importFrom utils head
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon xlab ylab ggtitle scale_fill_manual theme element_blank
#'
#' @references Christensen, K. B., Makransky, G. and Horton, M. (2017)
#' \emph{Critical Values for Yen’s Q3: Identification of Local Dependence in the Rasch Model Using Residual Correlations, Applied Psychological Measurement, Vol. 41(3), 178-194},\cr
#' \doi{https://doi.org/10.1177/0146621616677520}.\cr
#' Yen W. M. (1984)
#' \emph{Effects of local item dependence on the fit and equating performance of the three-parameter logistic model, Applied Psychological Measurement, Vol. 8, 125-145},\cr
#' \doi{10.18637/jss.v039.i05}.
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' set.seed(3)
#' object <- eRm::PCM(it.SPADI)
#' beta <- object$betapar
#' betamat <- PARmat(it.SPADI, beta)
#' pp <- eRm::person.parameter(object)
#' theta <- unlist(pp$thetapar)
#' sims <- simRASCHq3(beta = betamat, theta,
#'                    method.item = "CML", method.person = "MLE",
#'                    B = 10, model = "RMP", trace.it = 1)
#' plot(sims)
#'
#' @method plot RASCHq3
#' @rdname plot.RASCHq3
#' @export
#'
plot.RASCHq3 <- function(x, extreme = c("max", "min", "star"), probs, breaks, labels, colours, xtitle, title, col.outline = 0, alpha.ribbon = 1, title.legend = "Quantiles", ...) {

  if (!inherits(x, "RASCHq3")) {
    stop("use only with \"RASCHq3\" objects")
  }

  extreme <- match.arg(extreme)

  if (missing(probs)) {
    if (extreme == "min") {
      probs <- c(0.025, 0.05)
    }
    if (extreme %in% c("max", "star")) {
      probs <- c(0.95, 0.975)
    }
  }

  if (missing(breaks)) {
    if (extreme == "min") {
      breaks <- paste0(probs * 100, "%")
    }
    if (extreme %in% c("max", "star")) {
      breaks <- paste0((1-probs) * 100, "%")
    }
  }

  if (missing(labels)) {
    if (extreme == "min") {
      labels <- c(breaks, "other")
    }
    if (extreme %in% c("max", "star")) {
      labels <- c("other", breaks)
    }
  }

  if (missing(colours)) {
    colours <- c("#A43340", "#F44F4F", "#CFCFC2")
    names(colours) <- c(breaks, "other")
  }

  if (missing(xtitle)) {
    xtitle <- switch(extreme,
                "min" = expression(paste(Q["3,min"])),
                "max" = expression(paste(Q["3,max"])),
                "star" = expression(paste(Q["3,*"]))
                )
  }

  if (missing(title)) {
    title <- switch(extreme,
                     "min" = expression(paste(Distribution~of~Q["3,min"])),
                     "max" = expression(paste(Distribution~of~Q["3,max"])),
                     "star" = expression(paste(Distribution~of~Q["3,*"]))
    )
  }

  starfct <- function(x) max(x) - mean(x)

  z <- switch(extreme,
              "min" = unlist(lapply(x$statobj, function(x) min(x[upper.tri(x)]))),
              "max" = unlist(lapply(x$statobj, function(x) max(x[upper.tri(x)]))),
              "star" = unlist(lapply(x$statobj, function(x) starfct(x[upper.tri(x)]))))
  z <- z[!is.na(z)]

  dens <- density(z)
  quantiles <- quantile(z, prob = probs)
  ints <- findInterval(dens$x, quantiles)
  labels <- labels[unique(ints)+1]
  df <- data.frame(x = dens$x, y = dens$y,
                   quant = factor(findInterval(dens$x, quantiles),
                                  labels = labels))

  df$group <- rleid(df$quant)
  df_plot <- head(do.call(rbind, by(df, df$group, rbind, NA)), -1)
  df_plot[,c("group","quant")] <- lapply(df_plot[,c("group","quant")], na.locf)
  df_plot[] <- lapply(df_plot, na.locf, fromLast = TRUE)

  ggplot(data = df_plot, aes(.data$x, .data$y)) +
    #geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = .data$y,
                    fill = .data$quant, group = .data$group),
                colour = col.outline, alpha = alpha.ribbon) +
    xlab(xtitle) + ylab("") +
    ggtitle(title) +
    scale_fill_manual(values = colours, breaks = breaks, labels = breaks,
                      name = title.legend) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())

}

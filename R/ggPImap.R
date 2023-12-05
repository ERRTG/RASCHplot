#' Histogram for Person-Item Map
#'
#' Visualise the alignment of the item and person parameters.
#'
#' @param p A \link[RASCHplot]{ggImap} to add marginal plots to. If \code{p} is
#' not provided, then all of \code{theta}, and \code{delta} must be
#' provided.
#' @param theta The person parameters to use for creating the marginal plots.
#' @param delta Input delta (item-category threshold) parameters. This must be a matrix or list of matrices in case of stratification. Ignored if \code{object} is provided. Ignored if \code{p} is provided.
#' @param group group
#' @param type What type of marginal plot to show. One of: [density, histogram, boxplot, violin, densigram]
#' (a "densigram" is when a density plot is overlaid on a histogram).
#' @param margins Along which margins to show the plots. One of: [both, x, y].
#' @param size Integer describing the relative size of the marginal plots
#' compared to the main plot. A size of 5 means that the main plot is 5x wider
#' and 5x taller than the marginal plots.
#' @param ggtheme ggplot2 function or theme object. Default value is \code{theme_void}. Allowed values are the official ggplot2 themes including \code{theme_gray}, \code{theme_bw}, \code{theme_classic}, \code{theme_void}, .... Theme objects are also allowed (e.g., \code{theme_classic()}).
#' @param ... Extra parameters to pass to the marginal plots. Any parameter that
#' \code{geom_line()}, \code{geom_histogram()}, \code{geom_boxplot()}, or \code{geom_violin()} accepts
#' can be used. For example, \code{colour = "red"} can be used for any marginal plot type,
#' and \code{binwidth = 10} can be used for histograms.
#' @param xparams List of extra parameters to use only for the marginal plot along
#' the x axis.
#' @param yparams List of extra parameters to use only for the marginal plot along
#' the y axis.
#' @param groupColour If \code{TRUE}, the colour (or outline) of the marginal
#' plots will be grouped according to the variable mapped to \code{colour} in the
#' scatter plot. The variable mapped to \code{colour} in the scatter plot must
#' be a character or factor variable. See examples below.
#' @param groupFill If \code{TRUE}, the fill of the marginal
#' plots will be grouped according to the variable mapped to \code{colour} in the
#' scatter plot. The variable mapped to \code{colour} in the scatter plot must
#' be a character or factor variable. See examples below.
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_point scale_size guides scale_fill_gradient2 geom_text geom_label theme element_blank geom_label element_text coord_fixed theme_minimal coord_cartesian geom_histogram scale_color_discrete theme_void geom_density after_stat
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom ggstance position_dodgev
#' @importFrom gtable gtable_add_padding gtable_add_rows gtable_add_grob
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
#' object <- eRm::PCM(it.SPADI)
#' pp <- eRm::person.parameter(object)
#' p <- ggImap(object)
#' ggPImap(p, theta = unlist(pp$thetapar))
#'
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)
#' p <- ggImap(delta = delta)
#' ggPImap(p, theta = unlist(pp$thetapar))
#'
#' ggPImap(p, theta = unlist(pp$thetapar), group = as.factor(SPADI.complete$gender[-pp$pers.ex]))
#'
#' if (FALSE) {
#' SPADI1 <- SPADI[SPADI$gender == 1,]
#' SPADI2 <- SPADI[SPADI$gender == 2,]
#' SPADI.complet1 <- SPADI1[complete.cases(SPADI1), ]
#' SPADI.complet2 <- SPADI2[complete.cases(SPADI2), ]
#' it.SPADI1 <- SPADI.complet1[SPADI.complet1$gender == 1, 9:16]
#' it.SPADI2 <- SPADI.complet2[SPADI.complet2$gender == 2, 9:16]
#' object1 <- eRm::PCM(it.SPADI1)
#' object2 <- eRm::PCM(it.SPADI2)
#' beta1 <- object1$betapar
#' beta2 <- object2$betapar
#' pp1 <- eRm::person.parameter(object2) #eRm::person.parameter(object1)
#' pp2 <- eRm::person.parameter(object2)
#' theta1 <- unlist(pp1$thetapar)*rnorm(nrow(it.SPADI2))
#' theta2 <- unlist(pp2$thetapar)
#' threshtable1 <- eRm::thresholds(object1)$threshtable[[1]]
#' threshtable2 <- eRm::thresholds(object2)$threshtable[[1]]
#' objlist <- list(beta = list(beta1 = beta1, beta2 = beta2),
#'                 theta = list(theta1 = theta1, theta2 = theta2),
#'                 threshtable = list(threshtable1 = threshtable1,
#'                                    threshtable2 = threshtable2))
#' p <- ggImap(objlist, legend.title = "Gender", legend.labels = c("Male", "Female"))
#' ggPImap(p, theta = list(theta1 = theta1, theta2 = theta2),
#'              legend.title = "Gender", legend.labels = c("1", "2"))
#' }
#' @export
#'
ggPImap <- function(p, theta, delta, group = NULL,
                    type = c("histogram", "density", "boxplot", "violin", "densigram"),
                    margins = c("top", "right", "bottom", "left"),
                    size = 5, ggtheme = theme_void, ..., xparams = list(), yparams = list(),
                         groupColour = FALSE, groupFill = FALSE) {

  # Figure out all the default parameters.
  type <- match.arg(type, several.ok = TRUE)
  margins <- match.arg(margins)

  # Create one version of the scat plot (scatP), based on values of p, data, x,
  # and y...also remove all margin around plot so that it's easier to position
  # the density plots beside the main plot
  scatP <- .reconcileScatPlot(p, delta) +
    ggplot2::theme(plot.margin = grid::unit(c(0, 0, .25, .25), "cm"))

  if (inherits(theta, "list")) {

    thetalistdf <- lapply(theta, as.data.frame)
    thetahdf <- data.table::rbindlist(thetalistdf, idcol = "group")
    thetalong <- reshape2::melt(thetahdf, id.vars = c("group"))
  }
  if (!is.null(group)) {
    thetalong <- data.frame(value = theta, group = group)
  }

  if (inherits(theta, "list") || !is.null(group)) {
    h <- ggplot(thetalong, aes(x = .data$value, color = .data$group, fill = .data$group))

    if ("histogram" %in% type) {
      h <- h +
        geom_histogram(aes(y = after_stat(!!str2lang("density"))), position="identity", alpha=0.5)
    }
    if ("density" %in% type) {
      h <- h +
        geom_density(aes(color = .data$group), alpha=0.5)
    }

  } else {

    h <- ggplot(mapping = aes(x = theta))

    if ("histogram" %in% type) {
      h <- h +
        geom_histogram(aes(y = after_stat(!!str2lang("density"))), position="identity", alpha=0.5)
    }
    if ("density" %in% type) {
      h <- h +
        geom_density(alpha=0.5)
    }

  }






  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    h <- h + ggtheme() #theme_void() #
  } else if (class(ggtheme)[[1]] == "theme") {
    h <- h + ggtheme #theme_void #
  }


  scatPbuilt <- ggplot2::ggplot_build(scatP)

  # Pull out the plot title/subtitle if one exists and save it as a grob for
  # later use
  labels <- scatPbuilt$plot$labels
  #hasTitle <- (!is.null(labels$title) || !is.null(labels$subtitle))
  #if (hasTitle) {
  #  titleGrobs <- getTitleGrobs(p)
  #  scatP$labels$title <- NULL
  #  scatP$labels$subtitle <- NULL
  #}
  #if (!is.null(legend.title)) {
  #  scatPbuilt$labels$colour <- legend.title
  #}
  #if (!is.null(legend.labels)) {
  #  scatPbuilt$plot$plot_env$legend.title <- legend.labels
  #}



  ## Create the margin plots by calling genFinalMargPlot
  above_221 <- utils::packageVersion("ggplot2") > "2.2.1"
  if (above_221) {
    limits <- scatPbuilt$layout$panel_scales_x[[1]]$range$range
  } else {
    limits <- scatPbuilt$layout$panel_scales_x[[1]]$layout$panel_ranges[[1]]$x.range
  }

  h <- h + scale_x_continuous(limits = limits, oob = scales::squish)
  #if (TRUE) {
  #  plt <- MarginalPlot$new("y", type, scatPbuilt, prmL, groupColour, groupFill)
  #  right <- plt$build()
  #}

  # Now add the marginal plots to the scatter plot
  pGrob <- ggplot2::ggplotGrob(scatP)
  withCallingHandlers({
    suppressMessages({
      ggxtraTmp <- gtable_add_padding(
        pGrob, grid::unit(c(0, 0.5, 0, 0), "lines")
      )
      ggxtraNoTtl <- .addTopMargPlot(ggxtraTmp, h, size = 5)
    })
  }, warning = function(w) {
    if (grepl("did you forget aes", w, ignore.case = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })

  ## Add the title to the resulting ggExtra plot if it exists
  #if (hasTitle) {
  #  ggPImap <- addTitleGrobs(ggxtraNoTtl, titleGrobs)
  #} else {
    ggPImap <- ggxtraNoTtl
  #}

  # Add a class for S3 method dispatch for printing the ggExtra plot
  class(ggPImap) <- c("ggPImap", class(ggPImap))

  ggPImap
}
#' Print a ggPImap object
#'
#' \code{ggPImap} objects are created from \code{ggPImap}. This is the S3
#' generic print method to print the result of the scatterplot with its marginal
#' plots.
#'
#' @param x ggPImap object.
#' @param newpage Should a new page (i.e., an empty page) be drawn before the
#' ggPImap is drawn?
#' @param ... ignored
#' @seealso \code{\link{ggImap}}
#' @export
#' @keywords internal
print.ggPImap <- function(x, newpage = grDevices::dev.interactive(), ...) {
  if (newpage) grid::grid.newpage()
  if (isTRUE(getOption("rstudio.notebook.executing"))) {
    x <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::annotation_custom(x) +
      ggplot2::theme_void()
    print(x)
  } else {
    grid::grid.draw(x)
  }
}

#' Internal ggPImap functions
#' @param p p
#' @param delta delta
.reconcileScatPlot <- function(p, delta) {
  if (missing(p)) {
    if (missing(delta)) {
      stop("`delta` must be provided if `p` is not provided",
           call. = FALSE
      )
    }
    p <- ggImap(delta)
  }
  p
}
#' @param ggMargGrob ggMargGrob
#' @param top top
#' @param size size
#' @noRd
.addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- .getPanelPos(ggMargGrob)
  topMargG <- .getMargGrob(top)
  gt <- gtable_add_rows(
    x = ggMargGrob,
    heights = grid::unit(1 / size, "null"), pos = 0
  )
  gtable_add_grob(
    x = gt, grobs = topMargG, t = 1, b = 1,
    l = panelPos[["l"]], r = panelPos[["r"]],
    z = Inf, clip = "on", name = "topMargPlot"
  )
}
#' @param gtableGrob gtableGrob
#' @noRd
.getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  layDF[layDF$name == "panel", c("t", "l", "b", "r")]
}
#' @param margPlot margPlot
#' @noRd
.getMargGrob <- function(margPlot) {
  margG <- ggplot2::ggplotGrob(margPlot)
  gtable::gtable_filter(margG, pattern = "panel")
}

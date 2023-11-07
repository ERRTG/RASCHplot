#' Peron-Item Map
#'
#' Visualise the alignment of the item and person parameters.
#'
#' @param object Either a \code{eRm} or \code{Rm} object -- result of \link[eRm]{RM} or \link[eRm]{PCM}. Or a list including a vector of person parameters, a vector (for dichotomous items) or data frame or matrix (for polytomous items) of item-category (difficulty) parameters, and data frame or matrix with location and threshold parameters.
#' @param item.subset Subset of items to be plotted. Either a numeric vector indicating the column in X or a character vector indicating the column name. If "all", all items are plotted. The number of items to be plotted must be > 1.
#' @param ggtheme ggplot2 function or theme object. Default value is \code{theme_minimal}. Allowed values are the official ggplot2 themes including \code{theme_gray}, \code{theme_bw}, \code{theme_classic}, \code{theme_void}, .... Theme objects are also allowed (e.g., \code{theme_classic()}).
#' @param xlab.label character, label on x-axis of the graph.
#' @param title character, title of the graph.
#' @param legend.title ...
#' @param legend.labels ...
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_point scale_size guides scale_fill_gradient2 geom_text geom_label theme element_blank geom_label element_text coord_fixed theme_minimal coord_cartesian geom_histogram scale_color_discrete
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
#' beta <- object$betapar
#' pp <- eRm::person.parameter(object)
#' theta <- unlist(pp$thetapar)
#' threshtable <- eRm::thresholds(object)$threshtable[[1]]
#' ggPImap(object)
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
#' theta1 <- unlist(pp1$thetapar)
#' theta2 <- unlist(pp2$thetapar)
#' threshtable1 <- eRm::thresholds(object1)$threshtable[[1]]
#' threshtable2 <- eRm::thresholds(object2)$threshtable[[1]]
#' objlist <- list(beta = list(beta1 = beta1, beta2 = beta2),
#'                 theta = list(theta1 = theta1, theta2 = theta2),
#'                 threshtable = list(threshtable1 = threshtable1,
#'                                    threshtable2 = threshtable2))
#' ggPImap(objlist, legend.title = "Gender", legend.labels = c("1", "2"))
#' }
#' @export
#'
ggPImap <- function(object, item.subset = "all", ggtheme = theme_minimal, xlab.label = "", title = "", legend.title = NULL, legend.labels = NULL) {

  if (inherits(object, c("Rm", "eRm"))) {
    object$beta <- object$betapar
    # compute threshtable from beta for dichotomous models and item names
    if (is.null(object$theta)) {
      if (object$model == "RMD" || max(object$X,na.rm=TRUE) < 2){
        dRm <- TRUE
        threshtable <- cbind(beta, beta) * -1 # betapars are easiness parameteres
        rownames(threshtable) <- substring(rownames(threshtable), first=6, last=9999)
      } else {
        dRm <- FALSE
        threshtable <- eRm::thresholds(object)$threshtable[[1]]
      }
    }
    # person parameters unlist in case of several for NA groups
    if (is.null(object$theta)) {
      suppressWarnings(pp <- eRm::person.parameter(object))
      object$theta <- unlist(pp$thetapar)
    }

    # Extract subset of items to be plotted
    threshmat <- as.matrix(threshtable)
    if (is.character(item.subset)){
      if (length(item.subset) > 1 && all(item.subset %in% rownames(threshtable))) {
        threshmat <- threshmat[item.subset, ]
      } else if (length(item.subset)!=1 || !(item.subset=="all")) {
        stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
      }
    } else {
      if (length(item.subset) > 1 && all(item.subset %in% 1:nrow(threshmat)))
        threshmat <- threshmat[item.subset,]
      else
        stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
    }


    threshdf <- data.frame(Item = rownames(threshmat), threshmat)
    threshlong <- melt(threshdf, id.vars = c("Item", "Location"))


  }

  if (!inherits(object, c("Rm", "eRm")) && inherits(object, "list")) {
    if (length(object) != 3) {
      stop("...")
    } else {
      if (inherits(object$beta, "list")) {

        # Extract subset of items to be plotted
        threshmat <- lapply(object$threshtable, as.matrix)
        if (is.character(item.subset)){
          if (length(item.subset) > 1 && all(item.subset %in% rownames(object$threshtable[[1]]))) {
            threshmat <- lapply(threshmat, function(x) x[item.subset, ])
          } else if (length(item.subset)!=1 || !(item.subset=="all")) {
            stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
          }
        } else {
          if (length(item.subset) > 1 && all(item.subset %in% 1:nrow(object$threshtable[[1]])))
            threshmat <- lapply(threshmat, function(x) x[item.subset, ])
          else
            stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
        }

        threshlistdf <- lapply(object$threshtable, function(x) {
          data.frame(Item = rownames(x), x)}
          )
        threshdf <- data.table::rbindlist(threshlistdf, idcol = "index")
        threshlong <- reshape2::melt(threshdf, id.vars = c("index", "Item", "Location"))

        thetalistdf <- lapply(object$theta, as.data.frame)
        thetahdf <- data.table::rbindlist(thetalistdf, idcol = "index")
        thetalong <- reshape2::melt(thetahdf, id.vars = c("index"))

      } else {

        # Extract subset of items to be plotted
        threshmat <- as.matrix(threshtable)
        if (is.character(item.subset)){
          if (length(item.subset) > 1 && all(item.subset %in% rownames(threshtable))) {
            threshmat <- threshmat[item.subset, ]
          } else if (length(item.subset)!=1 || !(item.subset=="all")) {
            stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
          }
        } else {
          if (length(item.subset) > 1 && all(item.subset %in% 1:nrow(threshmat)))
            threshmat <- threshmat[item.subset,]
          else
            stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
        }


        threshdf <- data.frame(Item = rownames(threshmat), threshmat)
        threshlong <- reshape2::melt(threshdf, id.vars = c("Item", "Location"))

      }
    }
  }



  #if (sorted)
  #  threshmat<-threshmat[order(threshmat[,1],decreasing=FALSE),]

  #loc <- as.matrix(threshmat[,1])
  #threshmat <- as.matrix(threshmat[,-1])




  if (inherits(object$beta, "list")) {
    p <- ggplot(threshlong, aes(x = .data$value, y = .data$Item, color = .data$index))
    h <- ggplot(thetalong, aes(x = .data$value, fill = .data$index))
  } else {
    p <- ggplot(threshlong, aes(x = .data$value, y = .data$Item))
    h <- ggplot(mapping = aes(x = object$theta))
  }

  h <- h +
    geom_histogram()

  # Adding thresholds
  if (inherits(object$beta, "list")) {
    p <- p +
      geom_line(aes(color = .data$index), position = position_dodgev(height = 0.3)) +
      geom_point(aes(color = .data$index),
                 position=position_dodgev(height = 0.3),
                 shape=21, fill="white"#, color="black"
                 )

    # Adding location
    p <- p +
      geom_point(data = threshdf,
                 aes(x = .data$Location, y = .data$Item, color = .data$index),
                 #colour = c(loc.col, "red"),
                 position=position_dodgev(height = 0.3))
  } else {
    p <- p +
      geom_line() +
      geom_point(shape=21, fill="white")

    # Adding location
    p <- p +
      geom_point(data = threshdf,
                 aes(x = .data$Location, y = .data$Item))
  }


  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
    h <- h + ggtheme() #theme_void() #
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
    h <- h + ggtheme #theme_void #
  }

  # add titles
  if (title != "") {
    p <- p +
      ggtitle(title)
  }
  if (xlab.label != "") {
    p <- p +
      xlab(xlab.label)
  }
  # Edit legend title and labels
  if (!is.null(legend.title)) {
    p <- p +
      scale_color_discrete(name = legend.title)
  }
  if (!is.null(legend.labels)) {
    p <- p +
      scale_color_discrete(labels = legend.labels)
  }

  # Align plots by axis
  #xlim <- range(theta, threshlong$value, na.rm = TRUE)
  #p <- p + coord_cartesian(xlim = xlim)
  #h <- h + coord_cartesian(xlim = xlim)

  #gA <- ggplotGrob(h)
  #gB <- ggplotGrob(p)
  #grid::grid.newpage()
  #grid::grid.draw(rbind(gA, gB))

  #gridExtra::grid.arrange(h, p, ncol=1, nrow=2, heights=c(1, 4))

  scatPbuilt <- ggplot2::ggplot_build(p)

  # Pull out the plot title/subtitle if one exists and save it as a grob for
  # later use
  labels <- scatPbuilt$plot$labels
  hasTitle <- (!is.null(labels$title) || !is.null(labels$subtitle))
  if (hasTitle) {
    titleGrobs <- getTitleGrobs(p)
    scatP$labels$title <- NULL
    scatP$labels$subtitle <- NULL
  }

  ## Create the margin plots by calling genFinalMargPlot
  #if (TRUE) {
  #  plt <- MarginalPlot$new("y", type, scatPbuilt, prmL, groupColour, groupFill)
  #  right <- plt$build()
  #}

  # Now add the marginal plots to the scatter plot
  pGrob <- ggplot2::ggplotGrob(p)
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

  # Add the title to the resulting ggExtra plot if it exists
  if (hasTitle) {
    ggPImap <- addTitleGrobs(ggxtraNoTtl, titleGrobs)
  } else {
    ggPImap <- ggxtraNoTtl
  }

  # Add a class for S3 method dispatch for printing the ggExtra plot
  class(ggPImap) <- c("ggPImap", class(ggPImap))

  ggPImap


}


#' Print a ggPImap object
#'
#' \code{ggPImap} objects are created from \code{ggMarginal}. This is the S3
#' generic print method to print the result of the scatterplot with its marginal
#' plots.
#'
#' @param x ggPImap object.
#' @param newpage Should a new page (i.e., an empty page) be drawn before the
#' ggPImap is drawn?
#' @param ... ignored
#' @seealso \code{\link{ggMarginal}}
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
# Internal ggPImap functions
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

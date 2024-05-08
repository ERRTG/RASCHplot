#' Rain cloud plot
#'
#' Visualise the alignment of the item and person parameters.
#'
#' @param theta The person parameters to use for creating the marginal plots.
#' @param delta Input delta (item-category threshold) parameters.
#' This must be a matrix or list of matrices in case of stratification.
#' @param which.item Either \code{NULL} (default) for constructing a joint rain cloud plot for all items, an integer or vector of integers giving the item(s), for which separate plots should be constructed. Or a character string \code{"all"} for constructing separate plots for all items in the data.
#' @param rain.title Label of axis for the latent dimension.
#' @param cloud.title Label of axis for the person parameter distribution.
#' @param rain.colour The color of the outline of the points.
#' @param rain.fill The color of the fill of the points.
#' @param cloud.colour The color of outline of the density.
#' @param cloud.fill The color of fill of the density.
#' @param drop.size The size of the points relative to the binwidth.
#' @param facets.nrow,facets.ncol,facets.scales,facets.shrink,facets.labeller,facets.as.table,facets.drop,facets.dir,facets.strip.position Arguments to be passed to \link[ggplot2]{facet_wrap} to display thresholds item separately but in the same graph.
#'
#' @importFrom ggplot2 ggplot aes geom_density scale_x_continuous sec_axis theme element_blank facet_wrap
#' @importFrom ggdist stat_dots stat_halfeye
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @importFrom dplyr rename mutate slice
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' object <- eRm::PCM(it.SPADI)
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)[,-1]
#' pp <- eRm::person.parameter(object)
#' theta <- unlist(pp$thetapar)
#' pp <- eRm::person.parameter(object)
#'
#' rainyRASCH(theta, delta)
#'
#' rainyRASCH(theta, delta, which.item = 3:4)
#'
#' @export rainyRASCH
#'
rainyRASCH <- function(theta, delta, which.item = NULL, rain.title, cloud.title, rain.colour, rain.fill, cloud.colour, cloud.fill, drop.size,
                       facets.nrow = NULL,
                       facets.ncol = NULL,
                       facets.scales =  "free",
                       facets.shrink = TRUE,
                       facets.labeller = "label_value",
                       facets.as.table = TRUE,
                       facets.drop = TRUE,
                       facets.dir = "h",
                       facets.strip.position = "top") {

  if (missing(rain.title)) {
    rain.title <- "Latent dimension"
  }
  if (missing(cloud.title)) {
    cloud.title <- "Person parameter distribution"
  }
  if (missing(rain.colour)) {
    rain.colour <- "#8C97A9"
  }
  if (missing(cloud.colour)) {
    cloud.colour <- "#8C97A9"
  }
  if (missing(rain.fill)) {
    rain.fill <- "#8C97A9"
  }
  if (missing(cloud.fill)) {
    cloud.fill <- "#8C97A9"
  }
  if (missing(drop.size)) {
    drop.size <- 1.07
  }
  #thetadf <- data.frame(theta = theta, Group = "1")%>%
  if (is.null(which.item)) {
    itemdf <- data.frame(Item = "1")
  } else {
    itemdf <- data.frame(Item = paste0("Item ", which.item))
  }

  thetadf <- data.frame(theta = theta) |>
    dplyr::cross_join(itemdf)

  if (!is.null(which.item)) {
    if (is.double(which.item) | is.integer(which.item)) {
      if (any(which.item < 1) | any(which.item > nrow(delta))) {
        stop("some values of which.item are smaller than 1 or greater than number of items in the model")
      }
    }
    thr <- delta[which.item, ]
    rownames(thr) <- paste0("Item ", which.item)
  } else {
    thr <- delta
  }

  itemlong <- thr |>
    reshape2::melt(value.name = "delta") |>
    dplyr::rename("Item" = "Var1", "Thresholds" = "Var2")

  #p <- ggplot(itemlong, aes(x = .data$Item, y = .data$delta, fill = .data$Item))
  p <- ggplot(thetadf, aes(x = .data$theta))

  if (!is.null(which.item)) {
    p <- p +
      facet_wrap(. ~ .data$Item,
                 nrow = facets.nrow,
                 ncol = facets.ncol,
                 scales = facets.scales,
                 shrink = facets.shrink,
                 labeller = facets.labeller,
                 as.table = facets.as.table,
                 drop = facets.drop,
                 dir = facets.dir,
                 strip.position = facets.strip.position)
  }

  # Person parameter distribution
  p <- p +
    geom_density(fill = cloud.fill, colour = cloud.colour) +
    scale_x_continuous(rain.title,
                       sec.axis = sec_axis(~ . , name=cloud.title))

  # Thresholds
  p <- p +
    stat_dots(
    data = itemlong,
    mapping = aes(x = delta),
    # ploting on left side
    side = "bottom",
    # adjusting position
    justification = 1.01,
    # adjust grouping (binning) of observations
    binwidth = 0.2,
    # adjust layout
    colour = rain.colour,
    fill = rain.fill,
    dotsize = drop.size
  )

  p <- p + theme(axis.title.y = element_blank(),
                 axis.text.y=element_blank(),
                 axis.ticks.y=element_blank())

  p

}
#'
#' @describeIn beta2delta Transformation of delta (item-category threshold) parameters to beta parameters
#'
#' @export rainyRASCHgrp
#'
rainyRASCHgrp <- function(delta) {

  thr <- delta[, -1]
  itemlong <- reshape2::melt(thr, value.name = "delta") |>
    dplyr::rename("Item" = "Var1", "Thresholds" = "Var2")

  p <- ggplot(itemlong, aes(x = .data$Item, y = .data$delta, fill = .data$Item))

  # add half-violin from {ggdist} package
  p <- p + stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
    )

  p <- p + stat_dots(
      # ploting on left side
      side = "left",
      # adjusting position
      justification = 1.1,
      # adjust grouping (binning) of observations
      binwidth = 0.25
    )

  p <- p + coord_flip()
  p

  # require(ggsci)
  # colourCount = ncol(it.SPADI)
  # getPalette = colorRampPalette(pal_jama(palette = c("default"), alpha = 1)(7))
  # p <- rainyRASCH(delta)
  # p + scale_fill_manual(values = getPalette(colourCount)) +
  #   theme_minimal() +
  #   labs(title = "Rain Cloud Plot",
  #        y = "Thresholds") +
  #   theme(legend.position = "none")

}









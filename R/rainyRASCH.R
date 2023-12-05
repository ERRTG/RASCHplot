#' Histogram for Person-Item Map
#'
#' Visualise the alignment of the item and person parameters.
#'
#' @param theta The person parameters to use for creating the marginal plots.
#' @param delta Input delta (item-category threshold) parameters.
#' This must be a matrix or list of matrices in case of stratification.
#' @param rain.title Label of axis for the latent dimension.
#' @param cloud.title Label of axis for the person parameter distribution.
#' @param rain.colour The color of the outline of the points.
#' @param rain.fill The color of the fill of the points.
#' @param cloud.colour The color of outline of the density.
#' @param cloud.fill The color of fill of the density.
#' @param drop.size The size of the points relative to the binwidth.
#'
#' @importFrom ggplot2 ggplot aes geom_density scale_x_continuous sec_axis theme element_blank
#' @importFrom ggdist stat_dots stat_halfeye
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#' @importFrom dplyr rename mutate
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' object <- eRm::PCM(it.SPADI)
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)
#' pp <- eRm::person.parameter(object)
#' theta <- unlist(pp$thetapar)
#' pp <- eRm::person.parameter(object)
#'
#' rainyRASCH(theta, delta)
#'
#'
#' @export rainyRASCH
#'
rainyRASCH <- function(theta, delta, rain.title, cloud.title, rain.colour, rain.fill, cloud.colour, cloud.fill, drop.size) {

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

  thetadf <- data.frame(theta = theta, Group = "1")

  thr <- delta[, -1]
  itemlong <- melt(thr, value.name = "delta", ) |>
    rename("Item" = "Var1", "Thresholds" = "Var2") |>
    mutate(Group = "1")

  p <- ggplot(thetadf, aes(x = .data$theta))

  # Person parameter distribution
  p <- p +
    geom_density(fill = cloud.fill, colour = cloud.colour) +
    scale_x_continuous(rain.title,
                       sec.axis = sec_axis(~ . , name=cloud.title))

  # Thresholds
  p <- p + stat_dots(
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









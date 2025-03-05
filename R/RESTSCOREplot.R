#' Item restscore plot
#'
#' This function constructs visualization of the observed Gamma coefficient between the score of a single item and the total score of the remaining items compared with the corresponding expected Gamma coefficient under the Rasch model. These plots can be used to investigate item misfit and the association between item scores and restscores under the Rasch model.
#'
#' ...
#'
#' @param model A model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()}, \code{PCM()}, or \code{RSM()} from the \code{eRm} package.
#' @param color.expected Colour of expected restscore point and confidence interval.
#' @param color.observed Colour of observed restscore point.
#' @param size Colour of points.
#' @param linewidth Width of confidence interval.
#'
#' @import ggplot2
#' @importFrom rlang .data
#' @import iarm
#'
#' @return Item restscore plot
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' SPADI.comp <- SPADI[complete.cases(SPADI[, 4:8]),]
#' it.SPADI.pain <- SPADI.comp[, 4:8]
#'
#' model.SPADI.pain <- eRm::PCM(it.SPADI.pain)
#' strat.vars <- list(gender = SPADI.comp[, "gender"])
#'
#' br <- cut(rowSums(it.SPADI.pain), quantile(rowSums(it.SPADI.pain), seq(0, 1, 0.25)))
#' table(br)
#' strat.vars <- list(over60 = SPADI.comp[, "over60"])
#' lower.groups <- c(0, 10.5, 16, 19, 25)
#' DIFplot(model = model.SPADI.pain,
#'         which.item = 1,
#'         strat.vars = strat.vars,
#'         lower.groups = lower.groups)
#'
#' RESTSCOREplot(model.SPADI.pain)
#'
#' @export RESTSCOREplot
#'
RESTSCOREplot <- function(model, color.expected = "grey", color.observed = "orange", size = 4, linewidth = 1) {

  irs <- item_restscore(model)
  irs <- as.data.frame(cbind(Item = rownames(irs), irs))
  irs[, 2:6] <- apply(irs[, 2:6], 2, function(x) as.numeric(as.character(x)))
  irs$num <- seq_along(irs$Item)

  ggplot(irs, aes(x = num, y = observed)) +
    geom_segment(data = irs, aes(xend = num,
                                 y = expected - 1.96 * se,
                                 yend = expected + 1.96 * se),
                 color = color.expected, linewidth = linewidth) +
    geom_point(aes(x = num, y = expected), color = color.expected, size = size) +
    geom_point(color = color.observed, size = size) +
    scale_x_continuous(breaks = irs$num,label = irs$Item, name = NULL) +
    coord_flip() +
    theme_minimal() +
    ylab("Item restscore")

}




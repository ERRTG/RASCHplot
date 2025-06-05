#' Item restscore plot
#'
#' This function constructs visualization of the observed Gamma coefficient between the score of a single item and the total score of the remaining items compared with the corresponding expected Gamma coefficient under the Rasch model. These plots can be used to investigate item misfit and the association between item scores and restscores under the Rasch model.
#'
#' ...
#'
#' @param model Either a model object or a list of model objects of class \code{Rm} or \code{eRm} returned from the functions \code{RM()}, \code{PCM()}, or \code{RSM()} from the \code{eRm} package.
#' @param model.overall A model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()}, \code{PCM()}, or \code{RSM()} from the \code{eRm} package.
#' @param color.expected Colour of expected restscore point and confidence interval.
#' @param color.observed Colour of observed restscore point.
#' @param size Colour of points.
#' @param linewidth Width of confidence interval.
#' @param ... Additional arguments
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
#' RESTSCOREplot(model = model.SPADI.pain)
#'
#' RESTSCOREplot(model = model.SPADI.pain)  +
#'     scale_color_manual(values = c("Observed" = "#FF91BC", "Expected" = "black")) +
#'     theme_minimal()
#'
#' it.SPADI.pain.0 <- SPADI.comp[SPADI.comp$over60 == 0, 4:8]
#' it.SPADI.pain.1 <- SPADI.comp[SPADI.comp$over60 == 1, 4:8]
#'
#' model.SPADI.pain.0 <- eRm::PCM(it.SPADI.pain.0)
#' model.SPADI.pain.1 <- eRm::PCM(it.SPADI.pain.1)
#'
#' irt.list <- list("Under 60" = model.SPADI.pain.0,
#'                  "Over 60" = model.SPADI.pain.1)
#'
#' RESTSCOREplot(irt.list)
#' RESTSCOREplot(irt.list, model.overall = model.SPADI.pain)
#' RESTSCOREplot(irt.list) +
#'   theme_minimal() +
#'   theme(legend.title = element_blank()) +
#'   theme(strip.background = element_rect(color = "white", fill = "lightgrey")) +
#'   scale_color_manual(values = c("Observed" = "#FF91BC", "Expected" = "black"))
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
#' @export RESTSCOREplot
#'
RESTSCOREplot <- function(model, model.overall = NULL, color.expected = "grey", color.observed = "orange", size = 2, linewidth = 1, ...) {

  if ("list" %in% class(model)) {

    if (!is.null(model.overall)) {

      irs_overall <- item_restscore(model.overall)
      irs_overall <- as.data.frame(cbind(Item = rownames(irs_overall), irs_overall))
      irs_overall[, 2:6] <- apply(irs_overall[, 2:6], 2, function(x) as.numeric(as.character(x)))
      irs_overall$num <- seq_along(irs_overall$Item)

    }

    strat.names <- names(model)
    irs.list <- lapply(seq_along(model), function(i) {
      irs0 <- item_restscore(model[[i]])
      irs0 <- as.data.frame(cbind(Item = rownames(irs0), irs0))
      irs0[, 2:6] <- apply(irs0[, 2:6], 2, function(x) as.numeric(as.character(x)))
      irs0$Stratification <- strat.names[i] #i
      irs0$strat <- strat.names[i]
      if (!is.null(model.overall)) irs0$expected <- irs_overall$expected
      irs0
    })

    irs <- do.call(rbind, irs.list)

    irs_long <- irs |>
      tidyr::pivot_longer(c("observed", "expected")) |>
      dplyr::mutate(name = paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))) |>
      dplyr::mutate(id = dplyr::row_number())

    ggplot(irs_long, aes(x = Stratification, y = name, color = name)) +
      geom_segment(data = irs_long |> subset(name == "Expected"), aes(xend = Stratification,
                                                                      y = value - 1.96 * se,
                                                                      yend = value + 1.96 * se), ...) +
      geom_point(aes(x = Stratification, y = value), size = size, ...) +
      coord_flip() +
      ylab("Item restscore") +
      facet_grid(Item ~.) +
      theme(legend.title = element_blank())

  } else if ("eRm" %in% class(model)) {

    irs <- item_restscore(model)
    irs <- as.data.frame(cbind(Item = rownames(irs), irs))
    irs[, 2:6] <- apply(irs[, 2:6], 2, function(x) as.numeric(as.character(x)))
    irs$num <- seq_along(irs$Item)
    irs_long <- irs |>
      tidyr::pivot_longer(c("observed", "expected")) |>
      dplyr::mutate(name = paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))) |>
      dplyr::mutate(id = dplyr::row_number())

    ggplot(irs_long, aes(x = num, y = name, color = name)) +
      geom_segment(data = irs_long |> subset(name == "Expected"), aes(xend = num,
                                   y = value - 1.96 * se,
                                   yend = value + 1.96 * se), ...) +
      geom_point(aes(x = num, y = value), size = size, ...) +
      scale_x_continuous(breaks = irs_long$num, labels = irs_long$Item, name = NULL) +
      coord_flip() +
      ylab("Item restscore") +
      theme(legend.title = element_blank())

  }

}




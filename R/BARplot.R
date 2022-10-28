#' Bar plot
#'
#' This function constructs bar plots...
#'
#' ...
#'
#' @param data Input data matrix or data frame; rows represent indiviiduals, columns represent items. Or a model object of class \code{Rm} or \code{eRm} returned from the functions \code{RM()} or \code{PCM()} from the \code{eRm} package.
#' @param which.item Either an integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is \code{which.item = 1}. Or a character string \code{"all"}for constructing CICC plots for all items in the data.
#' @param freq 	Logical flag; if TRUE, the graphic is a representation of frequencies, the counts component of the result; if FALSE, percentages are plotted (so that each bar has a total area of 100). Defaults to TRUE.
#' @param addsums Logical flag for adding score totals per item to the plots. Defaults to FALSE.
#' @param na.action A character which indicates what should happen when the data contain missing scores. The default is \code{"na.plot"}, which shows NAs in the plot. Another possible value is \code{"na.rm"}, which removes NAs from the plot.
#' @param strat.var A vector of observations used for stratification.
#'
#' @importFrom ggplot2 ggplot aes remove_missing facet_grid theme xlab ylab coord_flip guides guide_legend geom_text position_stack geom_col vars
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr mutate count select
#' @import magrittr
#' @importFrom forcats fct_rev
#'
#' @return Bar plot
#'
#' @examples
#' library(eRm)
#' data(SPADI)
#' items <- SPADI[,9:16]
#' BARplot(data = items)
#' BARplot(data = items, which.item = c(3,4))
#' BARplot(data = items, addsums = TRUE)
#' BARplot(data = items, freq = FALSE)
#' BARplot(data = items, freq = FALSE, na.action = "na.rm")
#' strat.var <- as.factor(SPADI$over60)
#' BARplot(data = items, which.item = 2, strat.var = strat.var)
#'
#' @export BARplot
#'
BARplot <- function(data, which.item = "all", freq = TRUE, addsums = FALSE, na.action = c("na.plot", "na.rm", "na.report"), strat.var) {

  na.action <- match.arg(na.action)

  if (is.double(which.item) | is.integer(which.item)) {
    if (any(which.item < 1) | any(which.item > ncol(data))) {
      stop("some values of which.item are smaller than 1 or greater than number of items in the model")
    }
  }

  if(any(class(data) %in% c("Rm", "eRm"))) {
    df <- data$X
  } else if (any(class(data) == "matrix")) {
    df <- as.data.frame(data)
  } else {
    df <- data
  }

  if (!missing(strat.var)) {
    if (length(strat.var) != nrow(df)) {
      stop("lengths of stratification variable must equal number of rows in data input")
    }
  }

  if (missing(strat.var)) {

    longdf <- longdffct(df, which.item, freq)

    barplt(longdf, addsums, na.action, freq)

  } else {

    longdf <- longdffct(df, which.item, freq, strat.var)

    barplt(longdf, addsums, na.action, freq, strat.var)

  }

}
#' Internal data generating function
#' @param df df
#' @param which.item which.item
#' @param freq freq
#' @param strat.var strat.var
#' @noRd
longdffct <- function(df, which.item, freq, strat.var) {

  if(any(is.numeric(which.item)) & length(which.item) == 1) {

    if(missing(strat.var)) {

      longdf <- df %>%
        dplyr::select(dplyr::all_of(which.item)) %>%
        dplyr::mutate_at(c(which.item), factor) %>%
        dplyr::count(dplyr::across(dplyr::all_of(which.item))) %>%
        dplyr::mutate(q = .data$n/sum(.data$n) * 100)  %>%
        dplyr::mutate(item = colnames(df)[which.item]) %>%
        dplyr::rename_with(.cols = 1, ~"Score") %>%
        dplyr::mutate(Score = forcats::fct_rev(.data$Score))

    } else {

      dfs <- data.frame(df, strat.var = strat.var)
      colnames(dfs)[which.item] <- "Score"

      longdf <- dfs %>%
        dplyr::select(.data$Score, strat.var) %>%
        dplyr::mutate(dplyr::across(c(.data$Score, strat.var), factor)) %>%
        dplyr::count(dplyr::across(c(.data$Score, strat.var))) %>%
        dplyr::mutate(q = .data$n/sum(.data$n) * 100)  %>%
        dplyr::mutate(item = colnames(df)[which.item]) %>%
        dplyr::mutate(Score = forcats::fct_rev(.data$Score))

    }

  } else {

    if (all(which.item == "all")) {
      which.item <- 1:ncol(df)
    }

    if(missing(strat.var)) {
      longdf <- df %>%
        dplyr::select(dplyr::all_of(which.item)) %>%
        tidyr::pivot_longer(dplyr::any_of(colnames(df)[which.item]), names_to = "item", values_to = "Score") %>%
        dplyr::mutate(Score = as.factor(.data$Score)) %>%
        dplyr::mutate(Score = forcats::fct_rev(.data$Score)) %>%
        dplyr::count(.data$item, .data$Score) %>%
        dplyr::group_by(.data$item) %>%
        dplyr::mutate(q = .data$n/sum(.data$n) * 100)

    } else {

      dfs <- data.frame(df, strat.var = strat.var)

      longdf <- dfs %>%
        dplyr::select(c(dplyr::all_of(which.item), strat.var)) %>%
        tidyr::pivot_longer(dplyr::any_of(which.item), names_to = "item", values_to = "Score") %>%
        dplyr::mutate(Score = as.factor(.data$Score)) %>%
        dplyr::mutate(Score = forcats::fct_rev(.data$Score)) %>%
        dplyr::count(.data$item, strat.var, .data$Score) %>%
        dplyr::group_by(.data$item, strat.var) %>%
        dplyr::mutate(q = .data$n/sum(.data$n) * 100)

    }

  }

  longdf <- if(freq){
    dplyr::rename(longdf, y = .data$n)
  } else {
    dplyr::rename(longdf, y = q)
  }

  longdf
}
#' Internal BAR plot function for percentage
#' @param longdf longdf
#' @param addsums addsums
#' @param na.action na.action
#' @param freq freq
#' @param strat.var strat.var
#' @noRd
barplt <- function(longdf, addsums, na.action, freq, strat.var) {

  if (na.action %in% c("na.rm", "na.report")) {

    if (missing(strat.var)) {
      pp <- ggplot(remove_missing(longdf, na.rm = TRUE),
                   aes(x = .data$item, y = .data$y, fill = .data$Score, na.rm = TRUE))
    } else {
      pp <- ggplot(remove_missing(longdf, na.rm = TRUE),
                   aes(x = .data$strat.var, y = .data$y, fill = .data$Score, na.rm = TRUE)) +
        facet_grid(rows = vars(.data$item), switch = "y") +
        theme(strip.placement = "outside")
    }

  } else {

    if (missing(strat.var)) {
      pp <- ggplot(longdf, aes(x = .data$item, y = .data$y, fill = .data$Score))
    } else {
      pp <- ggplot(longdf, aes(x = .data$strat.var, y = .data$y, fill = .data$Score))+
        facet_grid(rows = vars(.data$item), switch = "y") +
        theme(strip.placement = "outside")
    }

  }

  if (freq) {
    pp <- pp +
      geom_col()
  } else {
    pp <- pp +
      geom_col(position = "fill")
  }

   pp <- pp +
    xlab(ifelse(nlevels(longdf$item) > 1, "Item", "")) +
    ylab(ifelse(freq, "Count", "Percentage")) +
    coord_flip() +
    theme(legend.position="bottom")  +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))

   if (addsums) {
     pp <- pp +
       geom_text(stat = "identity",
                 aes(label = round(.data$y, ifelse(freq, 0, 1)), y = .data$y),
                 position = position_stack(0.5))
   }

   pp
}

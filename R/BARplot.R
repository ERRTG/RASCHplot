#' Bar plot
#'
#' This function constructs bar plots...
#'
#' ...
#'
#' @param data Input data matrix or data frame; rows represent indiviiduals, columns represent items. Or a model object of class `Rm` or `eRm` returned from the functions `RM()` or `PCM()` from the `eRm` package.
#' @param which.item An integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is `which.item = 1`. The argument will not be used if `all.items = TRUE`.
#' @param freq 	Logical flag; if TRUE, the graphic is a representation of frequencies, the counts component of the result; if FALSE, percentages are plotted (so that each bar has a total area of 100). Defaults to TRUE.
#' @param addsums Logical flag for adding score totals per item to the plots. Defaults to FALSE.
#' @param na.action A character which indicates what should happen when the data contain missing scores. The default is `"na.plot"`, which shows NAs in the plot. Another possible value is `"na.rm"`, which removes NAs from the plot.
#' @param strat.var A vector of observations used for stratification.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr mutate count select
#' @import magrittr
#' @importFrom forcats fct_rev
#'
#' @return Bar plot
#'
#' @examples
#' items <- SPADI[,9:16]
#' theme_set(theme_minimal())
#' BARplot(data = items)
#' BARplot(data = items, addsums = TRUE)
#' BARplot(data = items, freq = FALSE)
#' BARplot(data = items, freq = FALSE, na.action = "na.rm")
#' strat.var <- as.factor(SPADI$over60)
#' BARplot(data = items, which.item = 1, strat.var = strat.var)
#'
#' @export BARplot
#'
BARplot <- function(data, which.item = "all", freq = TRUE, addsums = FALSE, na.action = c("na.plot", "na.rm", "na.report"), strat.var = NULL) {

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

  if (!is.null(strat.var) & length(strat.var) != nrow(df)) {
    stop("lengths of stratification variable must equal number of rows in data input")
  }

  if (is.null(strat.var)) {

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
longdffct <- function(df, which.item, freq, strat.var = NULL) {

  if(any(is.numeric(which.item)) & length(which.item) == 1) {

    if(is.null(strat.var)) {

      longdf <- df %>%
        dplyr::select(all_of(which.item)) %>%
        dplyr::mutate_at(c(which.item), factor) %>%
        dplyr::count(across(all_of(which.item))) %>%
        dplyr::mutate(q = n/sum(n) * 100)  %>%
        dplyr::mutate(item = colnames(df)[which.item]) %>%
        dplyr::rename_with(.cols = 1, ~"Score") %>%
        dplyr::mutate(Score = forcats::fct_rev(Score))

    } else {

      dfs <- data.frame(df, strat.var = strat.var)

      longdf <- dfs %>%
        dplyr::select(c(all_of(which.item), strat.var)) %>%
        dplyr::mutate_at(c(which.item), factor) %>%
        dplyr::count(across(c(all_of(which.item), strat.var))) %>%
        dplyr::mutate(q = n/sum(n) * 100)  %>%
        dplyr::mutate(item = colnames(df)[which.item]) %>%
        dplyr::rename_with(.cols = 1, ~"Score") %>%
        dplyr::mutate(Score = forcats::fct_rev(Score))

    }

  } else {

    if (all(which.item == "all")) {
      which.item <- 1:ncol(df)
    }

    if(is.null(strat.var)) {
      longdf <- df %>%
        dplyr::select(all_of(which.item)) %>%
        tidyr::pivot_longer(any_of(which.item), names_to = "item", values_to = "Score") %>%
        dplyr::mutate(Score = as.factor(Score)) %>%
        dplyr::mutate(Score = forcats::fct_rev(Score)) %>%
        dplyr::count(item, Score) %>%
        dplyr::group_by(item) %>%
        dplyr::mutate(q = n/sum(n) * 100)

    } else {

      dfs <- data.frame(df, strat.var = strat.var)

      longdf <- dfs %>%
        dplyr::select(c(all_of(which.item), strat.var)) %>%
        tidyr::pivot_longer(any_of(which.item), names_to = "item", values_to = "Score") %>%
        dplyr::mutate(Score = as.factor(Score)) %>%
        dplyr::mutate(Score = forcats::fct_rev(Score)) %>%
        dplyr::count(item, strat.var, Score) %>%
        dplyr::group_by(item, strat.var) %>%
        dplyr::mutate(q = n/sum(n) * 100)

    }

  }

  longdf <- if(freq){
    dplyr::rename(longdf, y = n)
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
barplt <- function(longdf, addsums, na.action, freq, strat.var = NULL) {

  if (na.action %in% c("na.rm", "na.report")) {

    if (is.null(strat.var)) {
      pp <- ggplot(remove_missing(longdf, na.rm = TRUE),
                   aes(x = item, y = y, fill = Score, na.rm = TRUE))
    } else {
      pp <- ggplot(remove_missing(longdf, na.rm = TRUE),
                   aes(x = strat.var, y = y, fill = Score, na.rm = TRUE)) +
        facet_grid(rows = vars(item), switch = "y") +
        theme(strip.placement = "outside")
    }

  } else {

    if (is.null(strat.var)) {
      pp <- ggplot(longdf, aes(x = item, y = y, fill = Score))
    } else {
      pp <- ggplot(longdf, aes(x = strat.var, y = y, fill = Score))+
        facet_grid(rows = vars(item), switch = "y") +
        theme(strip.placement = "outside")
    }

  }

   pp <- pp +
    geom_bar(stat = "identity") +
    xlab(ifelse(nlevels(longdf$item) > 1, "Item", "")) +
    ylab(ifelse(freq, "Count", "Percentage")) +
    coord_flip() +
    theme(legend.position="bottom")  +
    guides(fill = guide_legend(nrow = 1, reverse = TRUE))

   if (addsums) {
     pp <- pp +
       geom_text(stat = "identity",
                 aes(label = round(y, ifelse(freq, 0, 1)), y = y),
                 position = position_stack(0.5))
   }

   pp
}

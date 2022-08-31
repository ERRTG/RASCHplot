#' Bar plot
#'
#' This function constructs bar plots...
#'
#' ...
#'
#' @param data A model object of class `Rm` or `eRm` returned from the functions `RM()` or `PCM()` from the `eRm` package.
#' @param which.item An integer or vector of integers giving the item(s), for which a CICC-plot should be constructed. The default is `which.item = 1`. The argument will not be used if `all.items = TRUE`.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer everything
#' @importFrom dplyr mutate count select
#' @import magrittr
#'
#' @return Bar plot
#'
#' @examples
#' library(iarm)
#' str(amts)
#' it.AMTS <- amts[, 4:13]
#' theme_set(theme_minimal())
#' BARplot(it.AMTS)
#' data <- read.table("http://publicifsv.sund.ku.dk/~kach/scaleval/CAT.txt", sep=' ', header = TRUE)
#' @export BARplot
#'
BARplot <- function(data, which.item = "all") {

  if (is.double(which.item) | is.integer(which.item)) {
    if (any(which.item < 1) | any(which.item > ncol(data))) {
      stop("some values of which.item are smaller than 1 or greater than number of items in the model")
    }
  }

  if(any(is.numeric(which.item)) & length(which.item) == 1) {

    longdf <- data %>%
      dplyr::select(all_of(which.item)) %>%
      dplyr::mutate_at(c(which.item), factor) %>%
      dplyr::count(across(all_of(which.item))) %>%
      dplyr::mutate(n = n/sum(n) * 100)  %>%
      dplyr::mutate(item = colnames(data)[which.item]) %>%
      dplyr::rename_with(.cols = 1, ~"Score")

  } else {

    if (all(which.item == "all")) {
      which.item <- 1:ncol(data)
    }
    longdf <- data %>%
      dplyr::select(all_of(which.item)) %>%
      tidyr::pivot_longer(tidyr::everything(), names_to = "item", values_to = "Score") %>%
      dplyr::mutate(Score = as.factor(Score)) %>%
      dplyr::count(item, Score) %>%
      dplyr::group_by(item) %>%
      dplyr::mutate(n = n/sum(n) * 100)
  }

  ggplot(longdf, aes(x = item, y = n)) +
    geom_col(aes(fill = Score)) +
    xlab("Item") +
    ylab("Percentage") +
    coord_flip()


}

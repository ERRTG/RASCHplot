#' Heatmap plot function for Yen's Q3
#'
#' Visualise the Yen's Q3 statistics for a \code{Q3star} object using \link[ggcorrplot]{ggcorrplot} with mixed circle visualization (upper triangle) and numbers (lower triangle). The \eqn{Q_{3\ast}}{Q3*} value may be highlighted by a rectangle.
#'
#' @param object \code{Q3star} object, typically result of \link[RASCHplot]{Q3star}.
#' @param method character, the visualization method of Q3star matrix to be
#'   used. Allowed values are "circle" (default), "square".
#' @param type character, "mixed" (default), "full", "lower" or "upper" display.
#' @param markQ3star Logical flag for highlighting the \eqn{Q_{3\ast}}{Q3*} value by a rectangle.
#' @param lower character, the visualisation method of lower triangular part of Q3star matrix to be used (only valid when \code{type = "mixed"}). Allowed values are "number" (default), "circle", "square".
#' @param upper character, the visualisation method of upper triangular part of Q3star matrix to be used (only valid when \code{type = "mixed"}). Allowed values are "number", "circle" (default), "square".
#' @param ggtheme ggplot2 function or theme object. Default value is \code{theme_minimal}. Allowed values are the official ggplot2 themes including \code{theme_gray}, \code{theme_bw}, \code{theme_classic}, \code{theme_void}, .... Theme objects are also allowed (e.g., \code{theme_classic()}).
#' @param title character, title of the graph.
#' @param show.legend logical, if TRUE (default) the legend is displayed.
#' @param legend.title a character string for the legend title.
#' @param show.diag NULL or logical, whether display the correlation
#'   coefficients on the principal diagonal. If \code{NULL} (default), the default is to
#'   show diagonal correlation for \code{type = "full"} and to remove it when
#'   \code{type} is one of "upper" or "lower".
#' @param colors a vector of 3 colors for low, mid and high correlation values.
#' @param outline.color the outline color of square or circle. Default value is "gray".
#' @param lab logical value. If TRUE, add correlation coefficient on the plot.
#' @param lab_col,lab_size size and color to be used for the correlation
#'   coefficient labels. used when lab = TRUE.
#' @param pch add character on the glyphs of Q3star value (only valid when markQ3star is TRUE). Default value is 4.
#' @param pch.col,pch.cex the color and the cex (size) of pch (only valid when
#'   markQ3star is TRUE).
#' @param tl.cex,tl.col,tl.srt the size, the color and the string rotation of
#'   text label (variable names).
#' @param digits Decides the number of decimal digits to be displayed (Default is 2).
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_point scale_size guides scale_fill_gradient2 geom_text geom_label theme element_blank geom_label element_text coord_fixed theme_minimal
#' @importFrom rlang .data
#' @importFrom dplyr filter case_when
#' @importFrom grDevices colorRampPalette
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
#' q3obj <- Q3star(items = it.SPADI, method.item = "CML", method.person = "WML", model = "RMP")
#' ggQ3star(object = q3obj)
#' it.SPADI$D4D5 <- it.SPADI$D4 + it.SPADI$D5
#' it.SPADI.2 <- it.SPADI[,-c(4,5)]
#' q3obj2 <- Q3star(items = it.SPADI.2, method.item = "CML", method.person = "WML", model = "RMP")
#' ggQ3star(q3obj2)
#'
#' @export
#'
ggQ3star <- function(object, method = c("circle", "square"), type = c("mixed", "full", "lower", "upper"), markQ3star = c("circle", "pch"), lower = NULL, upper = NULL, ggtheme = theme_minimal, title = "", show.legend = TRUE, legend.title = "Q3star", show.diag = NULL, colors = NULL, outline.color = NULL, lab = FALSE, lab_col = "black", lab_size = 4, pch = 8, pch.col = "black", pch.cex = 5, tl.cex = 12, tl.col = "black", tl.srt = 45, digits = 2) {

  if (!inherits(object, "Q3star")) {
    stop("use only with \"Q3star\" objects")
  }

  markQ3star <- match.arg(markQ3star)
  type <- match.arg(type)
  method <- match.arg(method)
  if (is.null(show.diag)) {
    if (type == "full") {
      show.diag <- TRUE
    } else {
      show.diag <- FALSE
    }
  }

  if (type == "mixed") {
    if (is.null(lower)) {
      lower <- "number"
    }
    if (is.null(upper)) {
      upper <- method
    }
  }

  mypal <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF",
             "#6A6599FF", "#80796BFF")
  if (is.null(colors)) {
    colors <- c("#00A1D5FF", "white", "#DF8F44FF")
  }
  if (is.null(outline.color)) {
    outline.color <- "gray"
  }

  corrmat <- object$Q3matrix

  Q3nodiag <- corrmat
  diag(Q3nodiag) <- NA
  Q3mean <- mean(Q3nodiag, na.rm = TRUE)
  diag(corrmat) <- Q3mean

  corr <- reshape2::melt(corrmat - Q3mean, value.name = "value") %>%
    mutate(rid = as.integer(as.factor(.data$Var1)),
           cid = as.integer(as.factor(.data$Var2))) %>%
    mutate(part = case_when(
      .$rid < .$cid ~ "upper",
      .$rid == .$cid ~ "diag",
      .$rid > .$cid ~ "lower"
    )) %>%
    mutate(abs_corr = abs(.data$value),
           value.lab = round(value, digits),
           lab = .data$Var1)

  lims <- range(corr$value) * 1.1

  # heatmap
  #p <- corrlong %>%
  #  mutate(Var1 = factor(Var1, levels(corrlong$Var1)),
  #         Var2 = factor(Var2, levels(corrlong$Var1))) %>%
   p <- ggplot(data = corr %>% filter(part == "upper"),
              mapping = aes(x = .data$cid,
                            y = .data$rid,
                            fill = .data$value))

  # modification based on method
  if (method == "square") {
    p <- p +
      geom_tile(color = outline.color)
  } else if (method == "circle") {
    p <- p + geom_point(color = outline.color,
                        shape = 21,
                        aes(size = .data$abs_corr)) +
      scale_size(range = c(4, 10)) +
      guides(size = "none")
  }

  # adding colors
  p <- p + scale_fill_gradient2(
    low = colors[1],
    high = colors[3],
    mid = colors[2],
    midpoint = 0,
    limit = lims,
    name = legend.title
  )

  # matrix cell glyphs
  if (markQ3star == "pch") {
    p <- p + geom_point(
      mapping = aes(x = .data$Var1, y = .data$Var2),
      shape = pch,
      size = pch.cex,
      color = pch.col
    )
  }

  # adding values
  if (type == "full") {

    tri.dat <- corr %>%
      dplyr::filter(part != "diag")

  } else if (type == "lower" || lower == "number") {

    tri.dat <- corr %>%
      dplyr::filter(part == "lower")

    if (markQ3star == "circle") {
      Q3stardat <- corr %>%
        dplyr::filter(.$value == object$Q3max - Q3mean & .$part == "upper")
    }

  } else if (type == "upper" || upper == "number") {

    tri.dat <- corr %>%
      dplyr::filter(part == "upper")

    if (markQ3star == "circle") {
      Q3stardat <- corr %>%
        dplyr::filter(.$value == object$Q3max - Q3mean & .$part == "lower")
    }

  }

  p <- p +
    geom_text(data = tri.dat,
              aes(x = .data$cid, y = .data$rid), label = tri.dat$value.lab) +
    geom_text(data = Q3stardat,
              aes(x = .data$cid, y = .data$rid), label = Q3stardat$value.lab) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())

  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }


  p <- p +
    theme(
      axis.text.x = element_text(
        angle = tl.srt,
        vjust = 1,
        size = tl.cex,
        hjust = 1
      ),
      axis.text.y = element_text(size = tl.cex)
    ) +
    coord_fixed()


  # add titles
  if (title != "") {
    p <- p +
      ggtitle(title)
  }

  # add labels
  #labels <- levels(corr$Var1)
  #breaks <- seq(labels)
  #p <- p + scale_x_discrete(labels = labs)

  # removing legend
  if (!show.legend) {
    p <- p +
      theme(legend.position = "none")
  }

  # removing panel
  p <- p +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  p


}




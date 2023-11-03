#' Heatmap plot function for Yen's Q3
#'
#' Visualise the Yen's Q3 statistics for a \code{Q3star} object using \link[ggcorrplot]{ggcorrplot} with mixed circle visualization (upper triangle) and numbers (lower triangle). The \eqn{Q_{3\ast}}{Q3*} value may be highlighted by a rectangle.
#'
#' @param obj \code{Q3star} object, typically result of \link[RASCHplot]{Q3star}.
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
#' @importFrom dplyr filter
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
#' ggQ3star(obj = q3obj)
#' it.SPADI$D4D5 <- it.SPADI$D4 + it.SPADI$D5
#' it.SPADI.2 <- it.SPADI[,-c(4,5)]
#' q3obj2 <- Q3star(items = it.SPADI.2, method.item = "CML", method.person = "WML", model = "RMP")
#' ggQ3star(q3obj2)
#'
#' @export
#'
ggQ3star <- function(obj, method = c("circle", "square"), type = c("mixed", "full", "lower", "upper"), markQ3star = c("circle", "pch"), lower = NULL, upper = NULL, ggtheme = theme_minimal, title = "", show.legend = TRUE, legend.title = "Q3star", show.diag = NULL, colors = NULL, outline.color = NULL, lab = FALSE, lab_col = "black", lab_size = 4, pch = 8, pch.col = "black", pch.cex = 5, tl.cex = 12, tl.col = "black", tl.srt = 45, digits = 2) {

  if (!inherits(obj, "Q3star")) {
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
  #if (markQ3star) {
  #  p.mat <- matrix(nrow = nrow(obj$Q3matrix), ncol = ncol(obj$Q3matrix))
  #  idx <- which(obj$Q3matrix == obj$Q3max, arr.ind = TRUE)[,2]
  #  p.mat[idx[1], idx[2]] <- "*"
  #  p.mat[idx[2], idx[1]] <- "*"
  #}

  mypal <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF",
             "#6A6599FF", "#80796BFF")
  if (is.null(colors)) {
    colors <- c("#00A1D5FF", "white", "#DF8F44FF")
  }
  if (is.null(outline.color)) {
    outline.color <- "gray"
  }

  corr <- obj$Q3matrix

  Q3nodiag <- corr
  diag(Q3nodiag) <- NA
  idx <- which(Q3nodiag == obj$Q3max, arr.ind = TRUE)[,2]
  r <- colnames(corr)[idx]

  Q3mean <- mean(corr[upper.tri(corr, diag = FALSE)])
  diag(corr) <- Q3mean
  corr <- corr - Q3mean
  corr <- base::round(x = corr, digits = digits)
  lims <- corr

  col.lim <- range(lims) * 1.1

  if (!show.diag) {
    diag(corr) <- NA
  }

  # Get lower or upper triangle
  if (type == "lower" | (type == "mixed" & lower == method)) {
    corr[upper.tri(corr)] <- NA
    if (markQ3star == "circle") {
      corr[idx[1], idx[2]] <- corr[idx[2], idx[1]]
    }
    #p.mat <- data.frame(Var1 = r[2], Var2 = r[1], value = corr[idx[2], idx[1]])
  } else if (type == "upper" | (type == "mixed" & upper == method)) {
    corr[lower.tri(corr)] <- NA
    if (markQ3star == "circle") {
      corr[idx[2], idx[1]] <- corr[idx[1], idx[2]]
    }
    #p.mat <- data.frame(Var1 = r[1], Var2 = r[2], value = corr[idx[1], idx[2]])
  }

  # Melt corr and pmat
  corr <- reshape2::melt(corr, na.rm = TRUE)
  colnames(corr) <- c("Var1", "Var2", "value")
  #corr$pvalue <- rep(NA, nrow(corr))
  #corr$signif <- rep(NA, nrow(corr))

  corr$abs_corr <- abs(corr$value) * 10

  corr$lab <- TRUE
  if (type == "lower" | (type == "mixed" & lower == method)) {
    corr$lab[corr$Var1 == r[1] & corr$Var2 == r[2]] <- FALSE
  } else if (type == "upper" | (type == "mixed" & upper == method)) {
    corr$lab[corr$Var1 == r[2] & corr$Var2 == r[1]] <- FALSE
  }

  #p.mat$abs_corr <- abs(p.mat$value) * 10

  # heatmap
  p <- ggplot(data = corr,
              mapping = aes(x = Var1, y = Var2, fill = value))

  # modification based on method
  if (method == "square") {
    p <- p +
      geom_tile(color = outline.color)
  } else if (method == "circle") {
    p <- p + geom_point(color = outline.color,
                        shape = 21,
                        aes(size = abs_corr)) +
      scale_size(range = c(4, 10)) +
      guides(size = "none")
  }

  # adding colors
  p <- p + scale_fill_gradient2(
    low = colors[1],
    high = colors[3],
    mid = colors[2],
    midpoint = 0,
    limit = range(lims),
    name = legend.title
  )

  # matrix cell glyphs
  if (markQ3star == "pch") {
    p <- p + geom_point(
      data = p.mat,
      mapping = aes(x = Var1, y = Var2),
      shape = pch,
      size = pch.cex,
      color = pch.col
    )
  }

  # adding values
  if (type == "full") {
    p <- p +
      geom_text(aes(x = Var1, y = Var2),
                label = value,
                color = lab_col,
                size = lab_size)
  } else if (type == "lower" | lower == "number" ) {
    p <- p +
      geom_text(data = . %>% dplyr::filter(lab == TRUE), aes(x = Var2, y = Var1, label = value)) +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank())
  } else if (type == "upper" | upper == "number") {
    p <- p +
      geom_text(data = . %>% dplyr::filter(lab == TRUE), aes(x = Var2, y = Var1, label = value)) +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank())
  }


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




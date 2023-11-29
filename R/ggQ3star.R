#' Heatmap plot function for Yen's Q3
#'
#' Visualise the matrix of Yen's Q3 statistics for a \code{Q3} object using \link[ggplot2]{ggplot2}, e.g., with mixed circle visualization (upper triangle) and numbers (lower triangle). The \eqn{Q_{3\ast}}{Q3*} value may be highlighted by a rectangle. Great attention has been paid to details and appearance may be further tweaked through the function arguments or the `ggplot2` functionality.
#'
#' @param object \code{Q3} object, typically result of \link[RASCHplot]{Q3}.
#' @param method character, the visualization method of Q3 matrix to be
#'   used. Allowed values are "circle" (default), "square".
#' @param type character, "lower" (default), "upper", or "full" display.
#' @param labels character, "upper" (default), "lower", "full" or "none"/NULL, Q3 value labels.
#' @param markQ3star character, for highlighting the \eqn{Q_{3\ast}}{Q3*} value by a rectangle.
#' @param ggtheme ggplot2 function or theme object. Default value is \code{theme_minimal}. Allowed values are the official ggplot2 themes including \code{theme_gray}, \code{theme_bw}, \code{theme_classic}, \code{theme_void}, .... Theme objects are also allowed (e.g., \code{theme_classic()}).
#' @param title character, title of the graph.
#' @param show.legend logical, if TRUE (default) the legend is displayed.
#' @param legend.title a character string for the legend title.
#' @param show.diag NULL or logical, whether display the correlation
#'   coefficients on the principal diagonal. If \code{NULL} (default), the default is to
#'   show diagonal correlation for \code{type = "full"} and to remove it when
#'   \code{type} is one of "upper" or "lower".
#' @param colors a vector of 3 colors for low, mid and high correlation values.
#' @param outline.color the outline color of square or circle. Default value is "#CFCFC2".
#' @param outline.width the outline width of square or circle. Default value is 0 (no outline).
#' @param lab_col,lab_size size and color to be used for the correlation
#'   coefficient labels when showed.
#' @param pch add character on the glyphs of Q3star value (only valid when markQ3star is TRUE). Default value is 4.
#' @param pch.col,pch.cex the color and the cex (size) of pch (only valid when
#'   markQ3star is TRUE).
#' @param tl.cex,tl.col,tl.srt the size, the color and the string rotation of
#'   text label (variable names).
#' @param digits Decides the number of decimal digits to be displayed (Default is 2).
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_point scale_size guides scale_fill_gradient2 geom_text geom_label theme element_blank geom_label element_text coord_fixed theme_minimal scale_y_continuous scale_x_continuous
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
#' # Load data
#' data(SPADI)
#' # Remove incomplete cases
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' # Subset items
#' it.SPADI <- SPADI.complete[, 9:16]
#' # Compute Yen's Q3 statistics and the \eqn{Q_{3\ast}}{Q3*} value
#' q3obj <- Q3(items = it.SPADI, method.item = "CML", method.person = "WML", model = "RMP")
#' # Visualise Yen's Q3
#' ggQ3star(object = q3obj)
#' # The \eqn{Q_{3\ast}}{Q3*} value is obtained for items D4 and D5:
#' q3obj$Q3star
#'
#' # With the reasoning that these items hold the same information,
#' # we create a new variable as the sum of D4 and D5:
#' it.SPADI$D4D5 <- it.SPADI$D4 + it.SPADI$D5
#' # and remove the two:
#' it.SPADI.2 <- it.SPADI[,-c(4,5)]
#' # We compute new Yen's Q3 statistics:
#' q3obj2 <- Q3(items = it.SPADI.2, method.item = "CML", method.person = "WML", model = "RMP")
#' ggQ3star(q3obj2)
#'
#' @export
#'
ggQ3star <- function(object, method = c("circle", "square"), type = c("lower", "upper", "full"), labels = c(NA, "upper", "lower", "full", "none"), markQ3star = c("circle", "square", "pch", NA), ggtheme = theme_minimal, title, show.legend = TRUE, legend.title, show.diag, colors, outline.color, outline.width = 0, lab_col = "black", lab_size = 4, pch = 8, pch.col = "black", pch.cex = 2, tl.cex = 12, tl.col = "black", tl.srt = 45, digits = 2) {

  if (!inherits(object, "Q3")) {
    stop("use only with \"Q3\" objects")
  }


  markQ3star <- match.arg(markQ3star)
  type <- match.arg(type)
  method <- match.arg(method)
  labels <- match.arg(labels)

  if (is.na(labels)) {
    labels <- ifelse(type == "lower", "upper",
                     ifelse(type == "upper", "lower",
                            ifelse(type == "full" & markQ3star != "pch", "full","none")))
  }

  if (type == "full") {
    type <- c("lower", "upper")
  }

  if (labels == "full") {
    labels <- c("lower", "upper")
  }

  if (missing(show.diag)) {
    if (all(type == c("lower", "upper"))) {
      show.diag <- TRUE
    } else {
      show.diag <- FALSE
    }
  }

  if (missing(colors)) {
    colors <- c("#4575B4", "#CFCFC2", "#b24745")
  }
  if (missing(outline.color)) {
    outline.color <- "#CFCFC2"
  }

  if (missing(legend.title)) {
    legend.title <- expression(paste(Q["3,max"]))
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
           value.lab = round(.data$value, digits),
           lab = .data$Var1)

  #lims <- range(corr$value) * 1.1
  lims <- c(floor(min(corr$value)*100)/100, ceiling(max(corr$value)*100)/100)

  maxabsval <- ceiling(max(corr$abs_corr)*100)/100
  lims <- c(-maxabsval, maxabsval)

  tri.dat <- corr %>%
    dplyr::filter(.$part %in% labels | .$value == object$Q3max - Q3mean)

  if (markQ3star %in% c("circle", "square", "pch")) {
    Q3stardat <- corr %>%
      dplyr::filter(.$value == object$Q3max - Q3mean & !(.$part %in% c(type, "diag")))
  }


  p <- ggplot(data = corr %>% filter(.data$part %in% type),
              mapping = aes(x = .data$rid,
                            y = .data$cid,
                            fill = .data$value))

   # add labels
   p <- p + scale_x_continuous(breaks = seq(levels(corr$Var1)), labels = levels(corr$Var1))
   p <- p + scale_y_continuous(breaks = seq(levels(corr$Var1)), labels = levels(corr$Var1))

  # modification based on method
  if (method == "square") {
    p <- p +
      geom_tile(color = outline.color)
    p <- p + geom_tile(data = Q3stardat,
                       color = outline.color)
  } else if (method == "circle") {

    p <- p + geom_point(color = outline.color,
                        shape = 21, stroke = outline.width,
                        aes(size = .data$value))
    p <- p + geom_point(data = Q3stardat,
                        color = outline.color,
                        shape = 21, stroke = outline.width,
                        aes(size = .data$value))
  }

   p <- p + guides(fill=guide_legend(title=legend.title))
  # adding colors
  p <- p + scale_fill_gradient2(
    low = colors[1],
    high = colors[3],
    mid = colors[2],
    midpoint = 0,
    limit = lims,
    guide = "legend"
  )

  p <- p + guides(size=guide_legend(title=legend.title))

  # Change guide
  p <- p +
    scale_size(limits = lims, range = c(4, 14))

  # matrix cell glyphs
  if (all(type == c("lower", "upper")) && markQ3star == "pch") {
    p <- p + geom_point(
      data = corr %>%
        dplyr::filter(.$value == object$Q3max - Q3mean),
      mapping = aes(x = .data$rid, y = .data$cid),
      shape = pch,
      size = pch.cex,
      color = pch.col,
      show.legend = FALSE
    )
  }

  if (!is.null(labels) && !all(labels == "none")) {
    p <- p +
      geom_text(data = tri.dat,
                aes(x = .data$rid, y = .data$cid), label = tri.dat$value.lab,
                size = lab_size, color = lab_col) +
      geom_text(data = Q3stardat,
                aes(x = .data$rid, y = .data$cid), label = Q3stardat$value.lab,
                size = lab_size, color = lab_col) +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank())
  }
  if (all(labels == c("lower", "upper"))) {
    p <- p +
      geom_text(data = tri.dat,
                aes(x = .data$rid, y = .data$cid), label = tri.dat$value.lab,
                size = lab_size, color = lab_col) +
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
  if (!missing(title)) {
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
      axis.title.y = element_blank(), panel.grid.minor = element_blank()
    )
  p


}




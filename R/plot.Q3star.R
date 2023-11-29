#' Heatmap plot function for Yen's Q3
#'
#' Visualise the matrix of Yen's Q3 statistics for a \code{Q3} object, e.g., with mixed circle visualization (upper triangle) and numbers (lower triangle). The \eqn{Q_{3\ast}}{Q3*} value may be highlighted by a rectangle. Great attention has been paid to details and appearance may be further tweaked through the function arguments.
#'
#' @param x \code{Q3} object, typically result of \link[RASCHplot]{Q3}.
#' @param markQ3star Logical flag for highlighting the \eqn{Q_{3\ast}}{Q3*} value by a rectangle.
#' @param lower.col Passed as col parameter to the lower matrix.
#' @param upper.col Passed as col parameter to the upper matrix.
#' @param col.lim = NULL,
#' @param tl.pos Character or logical, position of text labels. If character, it must be one of 'lt', 'ld', 'td', 'd' or 'n'. 'lt' (default) means left and top, 'ld' means left and diagonal, 'td' means top and diagonal(near), 'l' means left, 'd' means diagonal, 'n' means don't add text-label.
#' @param tl.cex Numeric, for the size of text label (variable names).
#' @param tl.col The color of text label.
#' @param number.cex The cex parameter to send to the call to text when writing the correlation coefficients into the plot.
#' @param colQ3star Color of rectangle highlighting the \eqn{Q_{3\ast}}{Q3*} value.
#' @param ... Other paramters to be passed through to plotting functions. See \link[corrplot]{corrplot.mixed} for further details.
#'
#' @importFrom corrplot corrplot.mixed corrRect
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
#' q3obj <- Q3(items = it.SPADI, method.item = "CML", method.person = "WML", model = "RMP")
#' plot(q3obj)
#' it.SPADI$D4D5 <- it.SPADI$D4 + it.SPADI$D5
#' it.SPADI.2 <- it.SPADI[,-c(4,5)]
#' q3obj2 <- Q3(items = it.SPADI.2, method.item = "CML", method.person = "WML", model = "RMP")
#' plot(q3obj2)
#'
#' @method plot Q3
#' @rdname plot.Q3
#' @export
#'
plot.Q3 <- function(x, markQ3star = TRUE, lower.col = NULL, upper.col = NULL, col.lim = NULL, tl.pos = c('lt', 'ld', 'td', 'd', 'n'), tl.cex = 0.8, tl.col = "black", number.cex=0.8, colQ3star = "black", ...) {

  if (!inherits(x, "Q3")) {
    stop("use only with \"Q3\" objects")
  }

  tl.pos <- match.arg(tl.pos)

  mypal <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF",
             "#6A6599FF", "#80796BFF")

  corr <- x$Q3matrix
  Q3mean <- mean(corr[upper.tri(corr, diag = FALSE)])
  diag(corr) <- Q3mean
  lims <- corr - Q3mean
  Q3nodiag <- corr
  diag(Q3nodiag) <- NA
  idx <- which(Q3nodiag == x$Q3max, arr.ind = TRUE)[,2]
  r <- colnames(corr)[idx]

  if (is.null(lower.col)) {
    lower.col <- mypal[1]
  }
  if (is.null(upper.col)) {
    upper.col <- colorRampPalette(c(mypal[3],"white",mypal[2]))(200)
  }
  if (is.null(col.lim)) {
    col.lim <- range(lims)
  }

  corrRes <- corrplot.mixed(corr = corr - Q3mean,
                            is.corr = FALSE,
                            lower.col = lower.col,
                            upper.col = upper.col,
                            col.lim = col.lim,
                            tl.pos = tl.pos,
                            tl.cex = tl.cex,
                            tl.col = tl.col,
                            number.cex = number.cex,
                            ...)
  if (markQ3star) {
    corrRect(corrRes, namesMat = r, col = colQ3star)
  } else {
    corrRect(corrRes)
  }


}


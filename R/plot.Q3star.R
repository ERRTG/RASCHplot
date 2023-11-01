#' Heatmap plot function
#'
#' Visualise the Yen's Q3 statistics for a \code{Q3star} object.
#'
#' @param x \code{Q3star} object, typically result of \link[RASCHplot]{Q3star}.
#' @param ... Other paramters to be passed through to plotting functions.
#'
#' @importFrom corrplot corrplot.mixed corrRect
#' @importFrom grDevices colorRampPalette
#'
#' @references Christensen, K. B., Makransky, G. and Horton, M. (2017)
#' \emph{Critical Values for Yen’s Q3: Identification of Local Dependence in the Rasch Model Using Residual Correlations, Applied Psychological Measurement, Vol. 41(3), 178-194},
#' \doi{https://doi.org/10.1177/0146621616677520}.\cr
#' Yen W. M. (1984)
#' \emph{Effects of local item dependence on the fit and equating performance of the three-parameter logistic model, Applied Psychological Measurement, Vol. 8, 125-145},
#' \doi{10.18637/jss.v039.i05}.
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' it.SPADI <- SPADI.complete[, 9:16]
#' q3obj <- Q3star(items = it.SPADI, method.item = "CML", method.person = "WML", model = "RMP")
#' plot(q3obj)
#' it.SPADI$D4D5 <- it.SPADI$D4 + it.SPADI$D5
#' it.SPADI.2 <- it.SPADI[,-c(4,5)]
#' q3obj2 <- Q3star(items = it.SPADI.2, method.item = "CML", method.person = "WML", model = "RMP")
#' plot(q3obj2)
#'
#' @method plot Q3star
#' @rdname plot.Q3star
#' @export
#'
plot.Q3star <- function(x, ...) {

  if (!inherits(x, "Q3star")) {
    stop("use only with \"Q3star\" objects")
  }

  mypal <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF",
             "#6A6599FF", "#80796BFF")

  Q3mean <- mean(x$Q3matrix[upper.tri(x$Q3matrix, diag = FALSE)])

  corr <- x$Q3matrix
  diag(corr) <- Q3mean
  lims <- corr - Q3mean
  Q3nodiag <- x$Q3matrix
  diag(Q3nodiag) <- NA
  idx <- which(Q3nodiag == x$Q3max, arr.ind = TRUE)[,2]
  r <- colnames(x$Q3matrix)[idx]

  corrRes <- corrplot.mixed(corr - Q3mean, upper = 'circle', is.corr = FALSE,
                 lower = "number",
                 tl.pos = "lt",
                 tl.col = "black",
                 tl.cex = 0.8,
                 number.cex=0.8,
                 lower.col=mypal[1],
                 col.lim = range(lims),
                 upper.col = colorRampPalette(c(mypal[3],"white",mypal[2]))(200))
    corrRect(corrRes, namesMat = r)

}


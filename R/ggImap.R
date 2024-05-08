#' Person-Item Map
#'
#' Visualise the alignment of the item parameters.
#'
#' @param object Either a \code{eRm} or \code{Rm} object -- result of
#' \link[eRm]{RM} or \link[eRm]{PCM}. If \code{object} is not provided,
#' then \code{delta} must be provided.
#' @param delta Input delta (item-category threshold) parameters. This must be a matrix or list of matrices in case of stratification. Ignored if \code{object} is provided.
#' @param item.subset Subset of items to be plotted. Either a numeric vector indicating the column in X or a character vector indicating the column name. If "all", all items are plotted. The number of items to be plotted must be > 1.
#' @param ggtheme ggplot2 function or theme object. Default value is \code{theme_minimal}. Allowed values are the official ggplot2 themes including \code{theme_gray}, \code{theme_bw}, \code{theme_classic}, \code{theme_void}, .... Theme objects are also allowed (e.g., \code{theme_classic()}).
#' @param xlab.label character, label on x-axis of the graph.
#' @param title character, title of the graph.
#' @param legend.title ...
#' @param legend.labels ...
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile geom_point scale_size guides scale_fill_gradient2 geom_text geom_label theme element_blank geom_label element_text coord_fixed theme_minimal coord_cartesian geom_histogram scale_color_discrete
#' @importFrom rlang .data
#' @importFrom ggstance position_dodgev
#' @importFrom gtable gtable_add_padding gtable_add_rows gtable_add_grob
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
#' object <- eRm::PCM(it.SPADI)
#' ggImap(object)
#'
#' delta <- beta2delta(beta = object$betapar, x = it.SPADI)
#' ggImap(delta = delta)
#'
#' if (FALSE) {
#' SPADI1 <- SPADI[SPADI$gender == 1,]
#' SPADI2 <- SPADI[SPADI$gender == 2,]
#' SPADI.complet1 <- SPADI1[complete.cases(SPADI1), ]
#' SPADI.complet2 <- SPADI2[complete.cases(SPADI2), ]
#' it.SPADI1 <- SPADI.complet1[SPADI.complet1$gender == 1, 9:16]
#' it.SPADI2 <- SPADI.complet2[SPADI.complet2$gender == 2, 9:16]
#' object1 <- eRm::PCM(it.SPADI1)
#' object2 <- eRm::PCM(it.SPADI2)
#' beta1 <- object1$betapar
#' beta2 <- object2$betapar
#' pp1 <- eRm::person.parameter(object2) #eRm::person.parameter(object1)
#' pp2 <- eRm::person.parameter(object2)
#' theta1 <- unlist(pp1$thetapar)
#' theta2 <- unlist(pp2$thetapar)
#' threshtable1 <- eRm::thresholds(object1)$threshtable[[1]]
#' threshtable2 <- eRm::thresholds(object2)$threshtable[[1]]
#' objlist <- list(beta = list(beta1 = beta1, beta2 = beta2),
#'                 theta = list(theta1 = theta1, theta2 = theta2),
#'                 threshtable = list(threshtable1 = threshtable1,
#'                                    threshtable2 = threshtable2))
#' ggImap(objlist, legend.title = "Gender", legend.labels = c("1", "2"))
#' }
#' @export
#'
ggImap <- function(object, delta, item.subset = "all", ggtheme = theme_minimal, xlab.label = "", title = "", legend.title = NULL, legend.labels = NULL) {

  if (!missing(object)){
    if (inherits(object, c("Rm", "eRm"))) {
      # compute threshtable from beta for dichotomous models and item names
      if (is.null(object$threshtable)) {
        if (object$model == "RMD" || max(object$X,na.rm=TRUE) < 2){
          dRm <- TRUE
          delta <- cbind(object$betapar, object$betapar) * -1 # betapars are easiness parameteres
          rownames(delta) <- substring(rownames(delta), first=6, last=9999)
        } else {
          dRm <- FALSE
          delta <- eRm::thresholds(object)$threshtable[[1]]
        }
      } else {
        delta <- object$threshtable
      }
    }
  }

  if (inherits(delta, "list")) {

    # Extract subset of items to be plotted
    threshmat <- lapply(delta, as.matrix)
    if (is.character(item.subset)){
      if (length(item.subset) > 1 && all(item.subset %in% rownames(delta[[1]]))) {
        threshmat <- lapply(threshmat, function(x) x[item.subset, ])
      } else if (length(item.subset)!=1 || !(item.subset=="all")) {
        stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
      }
    } else {
      if (length(item.subset) > 1 && all(item.subset %in% 1:nrow(delta[[1]])))
        threshmat <- lapply(threshmat, function(x) x[item.subset, ])
      else
        stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
    }

    threshlistdf <- lapply(delta, function(x) {
      data.frame(Item = rownames(x), x)}
      )
    threshdf <- data.table::rbindlist(threshlistdf, idcol = "index")
    threshlong <- reshape2::melt(threshdf, id.vars = c("index", "Item", "Location"))


  } else {

    # Extract subset of items to be plotted
    threshmat <- as.matrix(delta)
    if (is.character(item.subset)){
      if (length(item.subset) > 1 && all(item.subset %in% rownames(delta))) {
        threshmat <- threshmat[item.subset, ]
      } else if (length(item.subset)!=1 || !(item.subset=="all")) {
        stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
      }
    } else {
      if (length(item.subset) > 1 && all(item.subset %in% 1:nrow(threshmat)))
        threshmat <- threshmat[item.subset,]
      else
        stop("item.subset misspecified. Use 'all' or vector of at least two valid item indices/names.")
    }


    threshdf <- data.frame(Item = rownames(threshmat), threshmat,
                           Location = rowMeans(threshmat))
    threshlong <- reshape2::melt(threshdf, id.vars = c("Item", "Location"))

  }


  #if (sorted)
  #  threshmat<-threshmat[order(threshmat[,1],decreasing=FALSE),]

  #loc <- as.matrix(threshmat[,1])
  #threshmat <- as.matrix(threshmat[,-1])

  if (inherits(delta, "list")) {
    p <- ggplot(threshlong, aes(x = .data$value, y = .data$Item, color = .data$index))
  } else {
    p <- ggplot(threshlong, aes(x = .data$value, y = .data$Item))
  }


  # Adding thresholds
  if (inherits(delta, "list")) {
    p <- p +
      geom_line(aes(color = .data$index), position = position_dodgev(height = 0.3)) +
      geom_point(aes(color = .data$index),
                 position=position_dodgev(height = 0.3),
                 shape=21, fill="white"#, color="black"
                 )

    # Adding location
    p <- p +
      geom_point(data = threshdf,
                 aes(x = .data$Location, y = .data$Item, color = .data$index),
                 #colour = c(loc.col, "red"),
                 position=position_dodgev(height = 0.3))
  } else {
    p <- p +
      geom_line() +
      geom_point(shape=21, fill="white")

    # Adding location
    p <- p +
      geom_point(data = threshdf,
                 aes(x = .data$Location, y = .data$Item))
  }


  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }

  # add titles
  if (title != "") {
    p <- p +
      ggtitle(title)
  }
  if (xlab.label != "") {
    p <- p +
      xlab(xlab.label)
  }
  # Edit legend title and labels
  if (!is.null(legend.title)) {
    p <- p +
      scale_color_discrete(name = legend.title)
  }
  if (!is.null(legend.labels)) {
    p <- p +
      scale_color_discrete(labels = legend.labels)
  }

  ggImap <- p

  # Add a class for S3 method dispatch for printing the ggImap plot
  class(ggImap) <- c("ggImap", class(ggImap))

  ggImap


}

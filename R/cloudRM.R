#' Simulate Data under a Rasch model
#'
#' Repeated random sampling to obtain item responses from estimated item and person parameters.
#'
#' @param items items
#' @param group group
#' @param method.item Estimation method for item parameters.
#' @param B Number of simulations.
#' @param model Character. "RMD" for dichotomous Rasch model. "RMP" for partial credit model.
#' @param trace.it If \code{trace.it=1}, then progress bars are displayed.
#'
#' @examples
#' data(SPADI)
#' SPADI.complete <- SPADI[complete.cases(SPADI), ]
#' group <- as.factor(SPADI.complete$gender)
#' SPADI1 <- SPADI.complete[group == 1,]
#' SPADI2 <- SPADI.complete[group == 2,]
#' it.SPADI1 <- SPADI1[, 9:16]
#' it.SPADI2 <- SPADI2[, 9:16]
#' items = SPADI.complete
#' B = 10
#'
#' @export cloud
cloud <- function(items, group, method.item = c("PCML", "CML", "JML", "MML"), B, model = c("RMD", "RMP"), trace.it = 0){

  method.item <- match.arg(method.item)
  model <- match.arg(model)

  obj <- vector(mode = "list", length = B)

  if (trace.it) cat("Simulating\n")
  for (b in 1:B) {

    id <- sample(group)
    strats <- levels(id)

    for (gr in strats) {

      #============= Fit item parameters =================================
      fit <- RASCHfits(method.item, method.person = NULL, items[])
      delta.sim <- fit$delta

      if (model == "RMD" || max(items,na.rm=TRUE) < 2){
        threshtable <- cbind(beta, beta) * -1 # betapars are easiness parameteres
        rownames(threshtable) <- substring(rownames(threshtable), first=6, last=9999)
      } else {
        threshtable <- eRm::thresholds(object)$threshtable[[1]]
      }


    }


    theta.sim <- fit$theta

    #============= Compute Q3 statistics =======================================

    resids <- RASCHresiduals(delta = delta.sim, theta = theta.sim, data = X[[b]])
    fitQ3 <- Q3(resids)
    statobj[[b]] <- fitQ3 #max(fitQ3) - mean(fitQ3)

    if (trace.it) message(paste(b)) #cat(sprintf("Iteration: %d/%d\n", b, B))
  }

  stats <- list(statobj = statobj,
                method.item = method.item,
                method.person = method.person)

  class(stats) <- "RASCHq3"
  stats

}

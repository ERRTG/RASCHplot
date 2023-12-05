SPADIraw <- read.csv("https://erda.ku.dk/public/archives/bacc560d26b01f7a65e77a9712a92e86/SPADI.csv")

itemsP <- SPADIraw[, 4:8]
itemsD <- SPADIraw[, 9:16]
idxP <- which(apply(itemsP, 1, function(x) sum(is.na(x))) == ncol(itemsP))
idxD <- which(apply(itemsD, 1, function(x) sum(is.na(x))) == ncol(itemsD))
SPADI <- SPADIraw[-c(idxP, idxD), ]

readr::write_csv(SPADI, "data-raw/SPADI.csv")
usethis::use_data(SPADI, overwrite = TRUE)

set.seed(1)
object <- eRm::PCM(it.SPADI)
SPADIdelta <- as.data.frame(beta2delta(beta = object$betapar, x = it.SPADI))
readr::write_csv(SPADIdelta, "data-raw/SPADIdelta.csv")
usethis::use_data(SPADIdelta, overwrite = TRUE)

pp <- eRm::person.parameter(object)
SPADItheta <- as.data.frame(pp$thetapar$NAgroup1)
readr::write_csv(SPADItheta, "data-raw/SPADItheta.csv")
usethis::use_data(SPADItheta, overwrite = TRUE)

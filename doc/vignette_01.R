## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  #echo = TRUE, message=FALSE,
  tidy = TRUE, 
  tidy.opts = list(blank = TRUE, arrow = TRUE), 
  highlight = TRUE,
  #collapse = FALSE,
  #cache.extra = R.version, autodep=TRUE,
  fig.width=6, fig.height=4, comment=NA,
  fig.align="center"
  )

library(tidyverse)

OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

theme_set(theme_minimal())

options(ggplot2.continuous.colour = OkabeIto)
options(ggplot2.continuous.fill = OkabeIto)
options(ggplot2.discrete.colour = OkabeIto)
options(ggplot2.discrete.fill = OkabeIto)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("ERRTG/RASCHplot", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(RASCHplot)

## -----------------------------------------------------------------------------
data(SPADI)

## -----------------------------------------------------------------------------
str(SPADI)

## -----------------------------------------------------------------------------
itemsIncomp <- SPADI[,9:16]

## -----------------------------------------------------------------------------
BARplot(itemsIncomp)

## -----------------------------------------------------------------------------
SPADI.complete <- SPADI[complete.cases(SPADI), ]
items <- SPADI.complete[, 9:16]

## -----------------------------------------------------------------------------
fit <- eRm::PCM(items)

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit)

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit, lower.groups = c(0,7,13,14,19,22,27,30,35))

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI.complete[, "gender"])
DIFplot(model = fit, strat.vars = strat.vars)

## ---- results = 'hide'--------------------------------------------------------
BARplot(itemsIncomp)

## ---- fig.height=1.5----------------------------------------------------------
BARplot(itemsIncomp, which.item = 1)

## ---- results = 'hide'--------------------------------------------------------
BARplot(itemsIncomp, freq = FALSE)

## ---- results = 'hide'--------------------------------------------------------
BARplot(itemsIncomp, na.action = "na.rm")

## ---- results = 'hide'--------------------------------------------------------
BARplot(itemsIncomp, addsums = TRUE)

## -----------------------------------------------------------------------------
strat.var.gender <- factor(SPADI$gender)
levels(strat.var.gender) <-  list("male" = 1, "female" = 2)

## ---- results = 'hide', fig.height=7.5----------------------------------------
BARplot(itemsIncomp, strat.var = strat.var.gender)

## ---- fig.height=2------------------------------------------------------------
BARplot(itemsIncomp, which.item = 2, strat.var = strat.var.gender)

## ---- fig.height=2------------------------------------------------------------
BARplot(itemsIncomp, which.item = 2, freq = FALSE, strat.var = strat.var.gender)

## ---- results = 'hide'--------------------------------------------------------
theme_set(theme_gray())
BARplot(itemsIncomp)

## -----------------------------------------------------------------------------
theme_set(theme_minimal())

## ---- results = 'hide'--------------------------------------------------------
barobj <- BARplot(itemsIncomp)

## ---- results = 'hide'--------------------------------------------------------
barobj + 
  scale_fill_brewer(palette="Set3", na.value = "grey")

## ---- results = 'hide'--------------------------------------------------------
barobj + theme(legend.position="top")

## ---- results = 'hide'--------------------------------------------------------
barobj + ggtitle("Distribution of item responses")

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit)

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit, lower.groups = c(1,5,15,20,30,35))

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit, error.bar = FALSE)

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit, which.item = c(3,4))

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit, which.item = "all")

## ---- results = 'hide', fig.height=16-----------------------------------------
CICCplot(model = fit, which.item = "all", grid.items = TRUE,
          common.legend = TRUE, ncol = 2, nrow = 4)

## ---- results = 'hide'--------------------------------------------------------
 p <- CICCplot(model = fit, which.item = c(1,2))
 p
 # Change colours for item 1
 p[[1]] +
   scale_colour_manual(values = c("burlywood","cornflowerblue"))
 # Change colours for item 2
 p[[2]] +
   scale_colour_manual(values = c("burlywood","cornflowerblue"))

## ---- results = 'hide'--------------------------------------------------------
theme_set(theme_gray())
CICCplot(fit)

## -----------------------------------------------------------------------------
theme_set(theme_minimal())

## ---- results = 'hide'--------------------------------------------------------
ciccobj <- CICCplot(fit)[[1]]

## ---- results = 'hide'--------------------------------------------------------
ciccobj +
   scale_colour_manual(values = c("burlywood","cornflowerblue"))

## ---- results = 'hide'--------------------------------------------------------
ciccobj + 
  scale_colour_manual(values = c("burlywood","cornflowerblue")) +
  xlim(c(3, 37))

## ---- results = 'hide'--------------------------------------------------------
ciccobj + 
  ylim(c(0,5))

## -----------------------------------------------------------------------------
ciccobj +
  scale_x_continuous(breaks = seq(0, 40, 5), minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:5, minor_breaks = NULL)   + 
  ylim(c(0,5))

## ---- results = 'hide'--------------------------------------------------------
ciccobj + 
  ylab("Item-score for item D1") + 
  ggtitle("")

## -----------------------------------------------------------------------------
ciccobj + 
  theme(plot.title = element_text(hjust = 0.5))

## -----------------------------------------------------------------------------
DIFplot(fit)

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI.complete[, "gender"])
DIFplot(model = fit, strat.vars = strat.vars)

## -----------------------------------------------------------------------------
DIFplot(model = fit, strat.vars = strat.vars, dodge.width = 0)

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI.complete[, "gender"], over60 = SPADI.complete[, "over60"])
DIFplot(model = fit, strat.vars = strat.vars)

## -----------------------------------------------------------------------------
strat.vars <- list(over60 = SPADI.complete[, "over60"])
DIFplot(model = fit, which.item = c(1,4), strat.vars = strat.vars)

## -----------------------------------------------------------------------------
lower.groups <- list(over60 = list("0" = c(0, 8, 13, 16, 32), "1" = c(0, 11, 16, 18, 26, 30)))
DIFplot(model = fit, which.item = c(1,4), strat.vars = strat.vars, lower.groups = lower.groups)

## -----------------------------------------------------------------------------
DIFplot(model = fit, which.item = c(1,2),
        strat.vars = strat.vars, lower.groups = lower.groups)


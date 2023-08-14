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

## ----setup--------------------------------------------------------------------
library(RASCHplot)

## -----------------------------------------------------------------------------
data(SPADI)

## -----------------------------------------------------------------------------
str(SPADI)

## -----------------------------------------------------------------------------
itemsIncomp <- SPADI[,9:16]

## -----------------------------------------------------------------------------
items <- SPADI[, 9:16]

## -----------------------------------------------------------------------------
fit <- eRm::PCM(items)

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


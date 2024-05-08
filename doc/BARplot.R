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

## ---- results = 'hide'--------------------------------------------------------
BARplot(itemsIncomp)

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


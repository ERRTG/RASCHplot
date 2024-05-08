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
BARplot(itemsIncomp)

## -----------------------------------------------------------------------------
items <- SPADI[, 9:16]

## -----------------------------------------------------------------------------
fit <- eRm::PCM(items)

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit)

## ---- results = 'hide'--------------------------------------------------------
CICCplot(fit, lower.groups = c(0,7,13,14,19,22,27,30,35))

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI[, "gender"])
DIFplot(model = fit, strat.vars = strat.vars)


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

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI[, "gender"])
DIFplot(model = fit, strat.vars = strat.vars)

## -----------------------------------------------------------------------------
DIFplot(fit)

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI[, "gender"])
DIFplot(model = fit, strat.vars = strat.vars)

## -----------------------------------------------------------------------------
DIFplot(model = fit, strat.vars = strat.vars, dodge.width = 0)

## -----------------------------------------------------------------------------
strat.vars <- list(gender = SPADI[, "gender"], over60 = SPADI[, "over60"])
DIFplot(model = fit, strat.vars = strat.vars)

## -----------------------------------------------------------------------------
strat.vars <- list(over60 = SPADI[, "over60"])
DIFplot(model = fit, which.item = c(1,4), strat.vars = strat.vars)

## -----------------------------------------------------------------------------
lower.groups <- list(over60 = list("0" = c(0, 8, 13, 16, 32), "1" = c(0, 11, 16, 18, 26, 30)))
DIFplot(model = fit, which.item = c(1,4), strat.vars = strat.vars, lower.groups = lower.groups)

## -----------------------------------------------------------------------------
DIFplot(model = fit, which.item = c(1,2),
        strat.vars = strat.vars, lower.groups = lower.groups)


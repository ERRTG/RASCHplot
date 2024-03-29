% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rRMP.R
\name{rRMP}
\alias{rRMP}
\title{Simulate item responses under a polytomous Rasch model}
\usage{
rRMP(delta, theta, B)
}
\arguments{
\item{delta}{Matrix K columns of item-category threshold parameters for K items with at most M categories and number of rows equal to (maximum) number of response categories and NA assigned to empty categories.}

\item{theta}{Input person parameters.}

\item{B}{Number of simulations.}
}
\description{
Repeated random sampling to obtain item responses from estimated item and person parameters.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{gt}
\alias{gt}
\alias{gte}
\alias{lt}
\alias{lte}
\alias{gt0}
\alias{gte0}
\alias{lt0}
\alias{lte0}
\title{Return proportion of the observations that are less or greater than some value}
\usage{
gt(x, y)

gte(x, y)

lt(x, y)

lte(x, y)

gt0(x)

gte0(x)

lt0(x)

lte0(x)
}
\value{
A numeric between 0 and 1
}
\description{
Return proportion of the observations that are less or greater than some value
}
\section{Functions}{
\itemize{
\item \code{gte}: Proportion of `x` greater or equal to `y`

\item \code{lt}: Proportion of `x` < `y`

\item \code{lte}: Proportion of `x` <= `y`

\item \code{gt0}: Proportion of `x` > 0

\item \code{gte0}: Proportion of `x` >= 0

\item \code{lt0}: Proportion of `x` < 0

\item \code{lte0}: Proportion of `x` <= 0
}}


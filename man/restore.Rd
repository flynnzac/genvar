% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preserve.r
\name{restore}
\alias{restore}
\title{restore a dataset from a previous preserve to be currently used}
\usage{
restore(envir, replace = FALSE)
}
\arguments{
\item{envir}{a previous preserve value.}

\item{replace}{if TRUE, restore even if another dataset is in memory.  If FALSE, do not.}
}
\value{
the preserved data set
}
\description{
restore a dataset from a previous preserve to be currently used
}
\examples{
require(stats)
use(cars, clear=TRUE)
p <- preserve()
collapse("mean(dist)","speed")
list()
restore(p, replace=TRUE)
list()
}

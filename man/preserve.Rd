% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preserve.r
\name{preserve}
\alias{preserve}
\title{preserve a data set before modification}
\usage{
preserve(data = NULL)
}
\arguments{
\item{data}{a data set to preserve}
}
\value{
a value that can be passed to \code{restore} to restore the data set later
}
\description{
preserve a data set before modification
}
\examples{
require(stats)
use(cars, clear=TRUE)
p <- preserve()
collapse("mean(dist)", "speed")
list()
restore(p, replace=TRUE)
list()
}

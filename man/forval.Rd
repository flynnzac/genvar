% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forval.r
\name{forval}
\alias{forval}
\title{Execute code in the datasets environment for all values of a vector, replacing a macro with the value in each iteration}
\usage{
forval(values, expr, macro = "\%val")
}
\arguments{
\item{values}{the vector of values to loop over.  For example, specifying 1:5 would loop over integers from 1 to 5.}

\item{expr}{a quoted expression (the experession must be enclosed in quotes) to evaluate in the loop which (presumably) uses the macro expression}

\item{macro}{a word to replace in the quoted expression with the values we are looping over (default: "\%val")}
}
\value{
returns NULL, invisibly
}
\description{
Execute code in the datasets environment for all values of a vector, replacing a macro with the value in each iteration
}
\examples{
use(cars, clear=TRUE)
listif()
forval (2:4, "gen(speed\%val, speed^\%val)")
listif()
}

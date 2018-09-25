## This file is part of rata.

## rata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## rata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with rata.  If not, see <https://www.gnu.org/licenses/>.



#' lists the names of the variables in the dataset
#'
#' @param pattern an optional regular expression which only returns variable names that match the expression
#' @examples
#' use(cars)
#' describe()
#' describe("s*")
#' @export
describe <- function(pattern=NULL)
{
  names <- eval(substitute({attr(data,"names")}),envir=data.env)
  attr(names, "type") <- eval(substitute({sapply(1:ncol(data), function (i) typeof(data[,i]))}),envir=data.env)
  class(names) <- "varlist"
  if (is.null(pattern))
    names
  else
  {
    toret <- attr(terms(varlist(pattern)),"term.labels")
    subset(names,toret)
  }
}

#' @export
subset.varlist <- function(x, vars)
{
  names <- x[x %in% vars]
  attr(names, "type") <- attr(x,"type")[x %in% vars]
  return (names)
}

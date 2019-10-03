## This file is part of genvar.

## genvar is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## genvar is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with genvar.  If not, see <https://www.gnu.org/licenses/>.



#' lists the names of the variables in the dataset
#'
#' @param pattern an optional regular expression which only returns variable names that match the expression
#' @return A vector of names of variables with an attribute called "type" giving the types of the variables.  The class of the object is "varlist". Sets getret("varlist") to this vector as well.
#' @examples
#' use(cars, clear=TRUE)
#' describe()
#' describe("s*")
#' @export
describe <- function(pattern=NULL)
{
  assert_loaded()
  names <- eval(substitute({attr(data,"names")}),envir=data.env)
  attr(names, "type") <- eval(substitute({sapply(1:ncol(data), function (i) typeof(data[,i]))}),envir=data.env)
  class(names) <- "varlist"
  if (is.null(pattern))
    setret("varlist", names)
  else
  {
    toret <- attr(terms(varlist(pattern)),"term.labels")
    setret("varlist", subset(names,toret))
  }

  getret("varlist")
}

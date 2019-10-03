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

#' gets a return value from genvar environment
#'
#' @param code the name of the value to get
#' @return returns the value corresponding to \code{code}.
#' @examples
#' use(cars, clear=TRUE)
#' describe()
#' getret("varlist")
#' @export
getret <- function (code)
{
  get(code, envir=ret.cache)
}

#' sets a return value in genvar environment
#'
#' @param code return value name
#' @param val value of object
#' @return returns NULL, invisibly
#' @examples
#' setret("string", "hello")
#' getret("string")
#' @export
setret <- function (code, val)
{
  assign(code, val, envir=ret.cache)
  invisible(NULL)
}


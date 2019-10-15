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

#' a function to take lags and leads with panel data
#'
#' a function to take lags and leads with panel data, mostly a wrapper for \code{plm}'s lag function.
#' @param x variable to lag
#' @param k how many lags to take?  If a negative number, leads will be generated.
#' @param ... other options to pass to \code{plm::lag}, does not need to be specified
#' @return returns lag of the variable as a data frame
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' xtset(year, state)
#' gen(Lemp, L(emp))
#' gen(L2emp, L(emp,2))
#' headdata(10)
#' @importFrom "plm" lag
#' @export
L <- function (x,k=1,...)
{
  varname <- deparse(substitute(x))
  if ("index" %in% eval(substitute(names(attributes(data)))))
  {
    eval(substitute(lag(data[,varname],k,...)), envir=data.env)
  } else {
    lag(x,k,...)
  }
}

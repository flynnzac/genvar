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

#' captures an expression, setting \code{getret("error")} to TRUE if there was an error and FALSE otherwise
#'
#' @param expr an expression to be evaluated
#' @param silent if TRUE,  suppress error messages from printing (default: FALSE)
#' @return FALSE if the expression successfully ran and TRUE otherwise also sets getret("error") to TRUE or FALSE as well
#' @examples
#' capture({log(1)})
#' getret("error")
#' capture({log(-1)})
#' getret("error")
#' @export
capture <- function (expr, silent=FALSE)
{
  val <- eval(substitute(try(expr,silent=silent)), envir=data.env)
  setret("error", class(val)=="try-error")

  class(val)=="try-error"
}


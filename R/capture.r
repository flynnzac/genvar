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

#' captures an expression, returning 1 if there was an error and zero otherwise
#'
#' @param expr an expression to be evaluated
#' @param silent if TRUE,  suppress error messages from printing (default: FALSE)
#' @return 0 if the expression successfully ran and 1 otherwise
#' @examples
#' capture({log(1)})
#' capture({log(-1)})
#' capture({log(x)}) # where x is not an already-created variable
#' @export
capture <- function (expr, silent=FALSE)
{
  val <- try(expr,silent=silent)
  return (ifelse(class(val)=="try-error", 1, 0))
}


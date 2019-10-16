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

#' captures an expression, returning TRUE if there was an error and FALSE otherwise
#'
#' @param expr an expression to be evaluated
#' @param silent if TRUE,  suppress error messages from printing (default: FALSE)
#' @return FALSE if the expression successfully ran and TRUE otherwise
#' @examples
#' capture({log(1)})
#' capture({log(-1)})
#' @export
capture <- function (expr, silent=FALSE)
{
  expr <- enquo(expr)
  val <- eval(substitute(try(eval_tidy(expr), silent=silent)),
              envir=data.env)
  class(val)=="try-error"
}


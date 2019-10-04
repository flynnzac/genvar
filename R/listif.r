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

#' prints the part of the dataset that satisfies certain conditions
#'
#' @param cond a conditional expression; only observations that satisfy the condition will be returned.
#' @param vars a variable list; only variables in the list will be returned.
#' @param ... other options, currently ignored
#' @return the part of the dataset that satisfies the condition and contains the specified columns
#' @export
listif <- function (cond=NULL, vars=NULL, ...)
{
  if (!is_loaded())
    return(invisible(NULL))
  
  cond <- cond
  vars <- vars
  if (is.null(vars))
    vars <- describe()
  else
  {
    if (!inherits(vars,"formula"))
      vars <- varlist(vars)
    vars <- attr(terms(vars), "term.labels")
  }
  if (is.null(cond))
    eval(substitute({data[,vars]}), envir=data.env)
  else
    eval(substitute({subset(data[,vars],with(data,eval(parse(text=cond))))}), envir=data.env)

}


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
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' listif(speed <= 20)
#' @export
listif <- function (cond, vars, ...)
{
  if (!is_loaded())
    return(invisible(NULL))

  if (missing(vars))
    vars <- describe()
  else
  {
    vars <- gvcharexpr(enquo(vars))
    vars <- structure_varlist(vars, type="vector")
  }
  if (missing(cond))
  {
    eval(substitute({data[,vars]}), envir=data.env)
  }  else {
    cond <- gvcharexpr(enquo(cond))
    
    eval(substitute({subset(data[,vars],with(data,eval(parse(text=cond))))}), envir=data.env)
  }

}


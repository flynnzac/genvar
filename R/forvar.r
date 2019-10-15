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

#' apply a function to each of a list of variables
#'
#' @param varlist a list of variables in the format "var1 var2 var3" (or, if a single variable, it may be unquoted).
#' @param action a quoted expression (must be quoted) to apply to each variable where the variable is represented in the expression by \code{macro}.
#' @param macro an expression that will be replaced in \code{action} for each variable, by default \%var.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' forvar("speed dist", "gen(%var2, %var^2)")
#' listif()
#' @export
forvar <- function (varlist, action, macro="%var")
{
  varlist <- gvcharexpr(enquo(varlist))
  varlist <- structure_varlist(varlist, type="vector")

  for (var in varlist)
  {
    expr <- gsub(macro, var, action)
    eval(parse(text=expr),
         envir=data.env)
  }

  invisible(NULL)
}

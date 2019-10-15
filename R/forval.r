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

#' Execute code in the datasets environment for all values of a vector, replacing a macro with the value in each iteration
#'
#' @param values the vector of values to loop over.  For example, specifying 1:5 would loop over integers from 1 to 5.
#' @param expr a quoted expression (the experession must be enclosed in quotes) to evaluate in the loop which (presumably) uses the macro expression
#' @param macro a word to replace in the quoted expression with the values we are looping over (default: "\%val")
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' forval (2:4, "gen(speed%val, speed^%val)")
#' listif()
#' @export
forval <- function (values, expr, macro="%val")
{
  for (val in values)
  {
    expr.sub <- gsub(macro, val, expr)
    eval(eval(parse(text=expr.sub)),
         envir=data.env)
  }

  invisible(NULL)
}

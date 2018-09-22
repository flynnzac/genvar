## This file is part of rata.

## rata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## rata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with rata.  If not, see <https://www.gnu.org/licenses/>.

#' apply a function to each of a list of variables and store the results
#' @export
#' @param varlist a list of variables in the format ~var1+var2+var3+... or as a vector of names like "var1 var2 var3".
#' @param action a quoted expression to apply to each variable where the word specified in \code{macro}.
#' @param macro an expression that will be replaced in \code{action} for each variable, by default %var.
#' @examples
#' use(cars)
#' forvar("speed dist", "gen('%var2', '%var^2')")
forvar <- function (varlist, action, macro="%var")
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }

  varlist <- as.Formula(varlist)
  varlist <- attr(terms(formula(varlist,lhs=0,rhs=1)), "term.labels")

  for (var in varlist)
  {
    expr <- gsub(macro, var, action)
    eval(parse(text=expr),
         envir=data.env)
  }
}

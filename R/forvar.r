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
#' @param varlist a list of variables in the format ~var1+var2+var3+... or as a vector of names like c("var1", "var2", "var3",...).
#' @param action an R function to apply to each variable, taking the variable name as its first argument.
#' @param ... any other options specified are passed to the action function.
#' @examples
#' use(cars)
#' 
forvar <- function (varlist, action, ...)
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }

  varlist <- as.Formula(varlist)
  varlist <- attr(terms(formula(varlist,lhs=0,rhs=1)), "term.labels")

  assign("action", action, envir=data.env)
  for (var in varlist)
  {
    eval(substitute(parse(text=paste("action(",var,",...)",sep=""))),
         envir=data.env)
  }
}

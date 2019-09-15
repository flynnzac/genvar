## This file is part of arata.

## arata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## arata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with arata.  If not, see <https://www.gnu.org/licenses/>.


#' convert a variable of another type into a string variable
#'
#' @param varlist variables to convert, either in the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @export
tostring <- function (varlist)
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }
  vars <- attr(terms(varlist), "term.labels")
  for (v in vars)
  {
    gen(v,paste("as.character(",v,")",sep=""),
        replace=TRUE)
  }
}

#' convert a variable with string type into a numeric value
#'
#' @param varlist variables to convert, either in the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @export
destring <- function (varlist)
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }

  vars <- attr(terms(varlist), "term.labels")
  desc <- subset(describe(), vars)
  for (v in 1:length(vars))
  {
    if (attr(desc,"type")[v] == "factor")
    {
      tostring(vars[v])
    }

    gen(vars[v],
        paste("as.numeric(", vars[v], ")",sep=""),
        replace=TRUE)

  }

}



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


#' convert a variable of another type into a string variable
#'
#' @param varlist variables to convert, either in the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' tostring("speed")
#' listif()
#' @export
tostring <- function (varlist)
{
  assert_loaded()
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

  invisible(NULL)
}

#' convert a variable with string type into a numeric value
#'
#' @param varlist variables to convert, either in the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' tostring("speed")
#' listif()
#' describe()
#' destring("speed")
#' listif()
#' describe()
#' @export
destring <- function (varlist)
{
  assert_loaded()
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
        paste("as.numeric(as.character(", vars[v], "))",sep=""),
        replace=TRUE)

  }

  invisible(NULL)

}



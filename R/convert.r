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
#' @param varlist variables to convert, in the form "var1 var2 var3", or if a single variable, var1 (unquoted) will work as well
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' tostring(speed)
#' listif()
#' @export
tostring <- function (varlist)
{
  assert_loaded()

  varlist <- gvcharexpr(enquo(varlist))
  varlist <- structure_varlist(varlist, type="vector")
  
  for (v in varlist)
  {
    gen(v,paste("as.character(",v,")",sep=""),
        replace=TRUE)
  }

  invisible(NULL)
}

#' convert a variable with string type into a numeric value
#'
#' @param varlist variables to convert, in the form "var1 var2 var3" or, if a single variable, an unquoted variable will work as well (i.e. var1).
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' tostring(speed)
#' listif()
#' describe()
#' destring(speed)
#' listif()
#' describe()
#' @export
destring <- function (varlist)
{
  assert_loaded()
  varlist <- gvcharexpr(enquo(varlist))

  varlist <- structure_varlist(varlist, type="vector")
  desc <- subset(describe(), varlist)
  for (v in 1:length(varlist))
  {
    if (attr(desc,"type")[v] == "factor")
    {
      tostring(varlist[v])
    }

    val <- paste("as.numeric(as.character(", varlist[v], "))",sep="")
    gen(varlist[v], val, replace=TRUE)

  }

  invisible(NULL)

}



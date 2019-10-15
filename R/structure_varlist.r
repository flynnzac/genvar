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

#' creates a formula object from a varlist, mostly for internal use.
#'
#' A varlist in \code{genvar} is a space-separated string potentially with wildcard characters, "var1 var2 var3 x*".  This function converts a varlist to a formula or to a vector.
#' @param x the varlist to be converted in "var1 var2 var3" format.  Can be specified using the \emph{globbing} characters "*" (match zero or more of any character) or "?" (match any single character) like "var*" or "var?" for "var1 var2 var3" or using regular expressions if \code{regex=TRUE} ("var[0-9]+" = "var1 var2 var3").
#' @param type if "formula", return a varlist in formula format; if "vector", return a varlist in character vector format.
#' @return a formula object which can be passed to \code{model.frame} or a character vector giving the name of each variable
#' @examples
#' use(cars, clear=TRUE)
#' structure_varlist("speed dist", type="formula")
#' structure_varlist("speed dist", type="vector")
#' structure_varlist("*", type="vector")
#' @export
structure_varlist <- function (x, type="formula")
{
  n <- describe()
  x <- strsplit(x, " ")[[1]]

  x <- gsub("\\*", "\\.*", x)
  x <- gsub("\\?", "\\.", x)

  x <- lapply(x, function (u)
  {
    if (grepl("\\.", u))
      n[grepl(u,n)]
    else
      u
  })


  x <- unique(unlist(x))
  if (type == "formula")
  {
    as.formula(paste("~",paste0(x,collapse="+"),sep=""))
  } else if (type == "vector") {
    x
  } else {
    stop("Unknown type of varlist.")
  }
  
}


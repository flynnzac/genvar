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
#' A varlist in \code{genvar} is either a space-separated string with wildcard characters, "var1 var2 var3 x*", or an R formula object ~var1+var2+var3+x1+x2....  This function converts from the more user-friendly space-separated string format to the formula format or to a vector of strings.
#' @param x the varlist to be converted in "var1 var2 var3" format.  Can be specified using the \emph{globbing} characters "*" (match zero or more of any character) or "?" (match any single character) like "var*" or "var?" for "var1 var2 var3" or using regular expressions if \code{regex=TRUE} ("var[0-9]+" = "var1 var2 var3").
#' @param type if "formula", return a varlist in formula format; if "vector", return a varlist in character vector format.
#' @return a formula object which can be passed to \code{model.frame} or a character vector giving the name of each variable
#' @export
varlist <- function (x, type="formula")
{
  n <- describe()
  x <- strsplit(x, " ")[[1]]

  x <- gsub("\\*", "\\.*", x)
  x <- gsub("\\?", "\\.", x)

  x <- sapply(x, function (u)
    ifelse(grepl("\\.",u), n[grepl(u,n)], u))

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


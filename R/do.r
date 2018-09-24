
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

#' Executes R code on the dataset
#'
#' Executes an R expression using variables from the dataset, possibly separately for each level of a given varlist (like the \code{by} prefix in Stata).
#' @param expr an R expression which can use any of the variable names in the current dataset
#' @param by a variable list in either "var1 var2 var3" format or in ~var1+var2+var3 format.  The R expression will be applied separately for the data subsetted to each level of the variable list.
#' @examples
#' use(cars)
#' do("{coef(lm(speed~dist))}")
#' @export
do <- function(expr, by=NULL)
{
  if (is.null(by))
  {
    eval(substitute({with(data, parse(text=expr))}),
         envir=data.env)
  } else {
    if (!inherits(by,"formula"))
    {
      by <- varlist(by)
    }
    eval(substitute({
      s <- split(data,interaction(model.frame(by)))
      lst <- lapply(s, function (u) with(u, parse(text=expr)))
      names(lst) <- names(s)
      lst
    }), envir=data.env)
  }
}


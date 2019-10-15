
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

#' Executes R code on the dataset
#'
#' Executes an R expression using variables from the dataset, possibly separately for each level of a given varlist (like the \code{by} prefix in Stata).
#' @param expr an R expression which can use any of the variable names in the current dataset.  It can be quoted or unquoted.
#' @param by a variable list in "var1 var2 var3" format or, if a single variable, it can be unquoted (var1).  The R expression will be applied separately for the data subsetted to each level of the variable list.
#' @return returns whatever the expression \code{expr} returns.  If \code{by} is specified, it will be a list of the result for applying the expression to each section of the data
#' @examples
#' use(cars, clear=TRUE)
#' do(coef(lm(speed~dist)))
#' @export
do <- function(expr, by)
{
  assert_loaded()

  expr <- gvcharexpr(enquo(expr))

  if (missing(by))
  {
    eval(substitute({with(data, eval(parse(text=expr)))}),
         envir=data.env)
  } else {
    by <- gvcharexpr(enquo(by))
    by <- structure_varlist(by, type="formula")

    eval(substitute({
      s <- split(data,interaction(model.frame(by)))
      lst <- lapply(s, function (u) with(u, eval(parse(text=expr))))
      names(lst) <- names(s)
      lst
    }), envir=data.env)
  }
}


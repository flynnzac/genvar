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

#' collapses a data set by variables using arbitrary aggregation functions
#'
#' collapse a data set to produce summary statistics possibly by a set of variables as in the Stata code: collapse (fun1) var1 (fun2) var2, by(byvar1 byvar2).  But this function is more flexible than the Stata version because any arbitrary function can be used in collapse not just traditional aggregation functions.
#' @param values an argument with the form \code{"fun1(var1) fun2(var2) fun3(var3,var4)"} describe the aggregations to be performed where fun1, fun2, fun3 are most likely aggregation functions like "sum", "mean", "max", "median", etc. But the function could be anything that returns a scalar.
#' @param byvar a variable list giving the variables to collapse by.  The resulting dataset will have as many rows as there are unique levels of the \code{byvar} variable list.
#' @return returns NULL, invisibly
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' listif()
#' collapse("sum(emp)","year")
#' listif()
#' @importFrom Formula "as.Formula"
#' @export
collapse <- function(values, byvar=NULL)
{
  assert_loaded()
  if (!inherits(values, "formula"))
  {
    values <- varlist(values)
  }

  if (!inherits(byvar, "formula"))
  {
    byvar <- varlist(byvar)
  }

  eval(substitute({

    collapse.exp <- strsplit(as.character(values)[-1],"\\+")[[1]]
    by.data <- model.frame(byvar, data=data, na.action=NULL)

    int <- interaction(by.data)
    s <- split(data,int)
    res <- sapply(s, function (u)
      with (u, sapply(collapse.exp, function (expr) eval(parse(text=expr)))))
    res <- as.matrix(res)
    if (min(dim(res))==1)
      res <- t(res)

    new.names <- collapse.exp
    for (n in 1:length(new.names))
    {
      by.data[,new.names[n]] <- res[n,match(as.character(int),names(s))]
    }
    use(unique(by.data), clear=TRUE)
  }), envir=data.env)

  invisible(NULL)

}

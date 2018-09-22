## This file is part of rtata.

## rtata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## rtata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with rtata.  If not, see <https://www.gnu.org/licenses/>.

library(Formula)

#' collapses a data set by variables using arbitrary aggregation functions
#'
#' @param form an argument with the form ~fun1(var1)+fun2(var2)+fun3(var3)+...|byvar1+byvar2+... . where fun1, fun2, and fun3 are aggregation functions like "mean", "sum", "max", etc.  \code{data} will contain all unique levels of (byvar1,byvar2,...) and fun1(var1),fun2(var2) evaluated on the subset of the data set with that value of the by variables.  The equivalent Stata is: collapse (fun1) var1 (fun2) var2 (fun3) var3 ..., by(byvar1 byvar2)
#' @examples
#' data(Produc)
#' use(Produc)
#' listif()
#' collapse(~sum(emp)|year)
#' listif()
#' @importFrom Formula "as.Formula"
#' @export
collapse <- function(form)
{
  form <- as.Formula(form)

  eval(substitute({
    collapse.exp <- strsplit(as.character(formula(form,lhs=0,rhs=1))[-1],"\\+")[[1]]

    by.data <- model.frame(formula(form,lhs=0,rhs=2), data=data, na.action=NULL)

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
    use(unique(by.data))
  }), envir=data.env)

  ls.res <- ls(all=TRUE,envir=data.env)

  rm(list=ls.res[ls.res!="data"],envir=data.env)
}

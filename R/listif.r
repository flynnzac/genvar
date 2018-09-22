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

#' prints the part of the dataset that satisfies certain conditions
#' 
#' @return the part of the dataset that satisfies the condition and contains the specified columns
#' @export
listif <- function (cond=NULL, ...)
{
  if (is.null(cond))
    eval(substitute({data}), envir=data.env)
  else
    UseMethod("listif", cond)
}

#' @export
listif.character <- function (cond)
{
  eval(substitute({subset(data,with(data,eval(parse(text=cond))))}), envir=data.env)
}

#' @export
listif.formula <- function (cond, ifstmt=NULL)
{
  cond <- as.Formula(cond)
  vars <- attr(terms(formula(cond,lhs=0,rhs=1)), "term.labels")
  if (is.null(ifstmt))
    eval(substitute({data[,vars]}),envir=data.env)
  else
    eval(substitute({subset(data[,vars],with(data,eval(parse(text=ifstmt))))}), envir=data.env)
}

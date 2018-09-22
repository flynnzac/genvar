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


#' drops variables or rows from the dataset
#'
#' @param x either a variable list in the format - ~var1+var2+var3 - in which case the listed variables are removed from the dataset, or a condition like: "var1==2" in which case observations that satisfy the condition are removed.
#' @examples
#' use(cars)
#' listif()
#' drop(~speed)
#' listif()
#' use(cars)
#' drop("speed <= 20")
#' listif()
#'@export
#'
drop <- function (x)
{
  UseMethod("drop",x)
}

#'@export
drop.character <- function (x)
{

  eval(substitute({
    rows <- with(data, which(eval(parse(text=x))))
    data <- data[-rows,]}), envir=data.env)
}

#'@export
drop.formula <- function (x)
{
  form <- as.Formula(x)
  vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
  eval(substitute({data[,vars] <- NULL}),envir=data.env)
}

#' keeps some variables or rows in the dataset and drops the rest
#'
#' 
#' @param x either a variable list in the format - ~var1+var2+var3 - in which case the listed variables are kept in the dataset, the other variables removed, or a condition like: "var1==2" in which case observations that satisfy the condition are kept and all others are removed.
#' @examples
#' use(cars)
#' listif()
#' keep(~speed)
#' listif()
#' use(cars)
#' keep("speed <= 20")
#' listif()
#' @export

keep <- function (x)
{
  UseMethod("keep",x)
}

#'@export
keep.character <- function (x)
{
  rows <- with(data, which(eval(parse(text=x))))
  eval(substitute({data <- data[rows,]}), envir=data.env)
}

#'@export
keep.formula <- function (x)
{
  form <- as.Formula(x)
  vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
  eval(substitute({data <- data[,vars]}),envir=data.env)
}

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


#' drops rows from the dataset
#'
#' @param x a condition like (ex: "var1==2") describing the observations that should be removed from the data set.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' dropif("speed <= 20")
#' listif()
#'@export
dropif <- function (x)
{
  assert_loaded()
  eval(substitute({
    rows <- with(data, which(eval(parse(text=x))))
    data <- data[-rows,]}), envir=data.env)
  postuse()
  invisible(NULL)
}

#' drops variables in varlist format from the dataset
#' @param x a varlist either in "var1 var2 var3" format or ~var1+var2+var3 format.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' dropvar("speed")
#' listif()
#' use(cars, clear=TRUE)
#' dropvar(~speed)
#' listif()
#' @export
dropvar <- function (x)
{
  assert_loaded()
  if (!inherits(x,"formula"))
  {
    x <- varlist(x)
  }

  form <- as.Formula(x)
  vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
  eval(substitute({data[,vars] <- NULL}),envir=data.env)
  postuse()
  invisible(NULL)
}

#' keeps some rows in the dataset and drops the rest
#'
#'
#' @param x a condition like: "var1==2" in which case observations that satisfy the condition are kept and all others are removed.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' keepif("speed <= 20")
#' listif()
#' @export
keepif <- function (x)
{
  assert_loaded()
  rows <- eval(substitute(with(data, which(eval(parse(text=x))))),
               envir=data.env)
  eval(substitute({data <- data[rows,]}), envir=data.env)
  postuse()
  invisible(NULL)
}

#' keeps some variables in the dataset and drops the others
#'
#' @param x a varlist either of the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' keepvar("speed")
#' listif()
#' use(cars, clear=TRUE)
#' keepvar(~speed)
#' listif()
#'@export
keepvar <- function (x)
{
  assert_loaded()
  if (!inherits(x,"formula"))
  {
    x <- varlist(x)
  }
  form <- as.Formula(x)
  vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")

  eval(substitute({data <- as.data.frame(data[,vars])}),envir=data.env)
  postuse()
  invisible(NULL)
}

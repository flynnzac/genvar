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


library(foreign)
library(readstata13)
library(tools)
#' @importFrom tools "file_ext"
#' @importFrom readstata13 "read.dta13"
#' @importFrom foreign "read.dta"
#' @importFrom utils "read.csv"
#' @importFrom utils "read.table"
#' @export
use <- function (x,...)
{
  UseMethod("use", x)
}

data.env <- new.env()

#' @export
use.data.frame <- function (x,...)
{
  assign("data", x, envir=data.env)
  postuse()
}

#' @export
use.character <- function (x, type=NULL, ...)
{
  if (is.null(type))
    type <- file_ext(x)

  if (type=="csv")
    assign("data", read.csv(x,...), envir=data.env)

  if (type=="dta")
    tryCatch({assign("data", read.dta(x,...), envir=data.env)},
             error=function (e)
             { assign("data", read.dta13(x), envir=data.env)})

  if (type=="txt")
    assign("data", read.table(x,...), envir=data.env)

  if (type=="tab")
    assign("data", read.csv(x,sep="\t",...), envir=data.env)

  if (!exists("data", envir=data.env))
    stop("Could not determine type of data. Did not load data.")
  else
    postuse()
}

postuse <- function()
{
  eval(substitute({data$rownum <- 1:nrow(data) }),
       envir=data.env)
}

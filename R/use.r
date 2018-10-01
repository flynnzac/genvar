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


library(foreign)
library(readstata13)
library(tools)

#' uses a dataset, marking it as the active dataset
#'
#' @param x usually either a data.frame or a csv/dta filename to be imported.  An R function which returns a data.frame can also be specified.
#' @param clear if TRUE, erase current data if it already exists (default: FALSE).
#' @importFrom tools "file_ext"
#' @importFrom readstata13 "read.dta13"
#' @importFrom foreign "read.dta"
#' @importFrom utils "read.csv"
#' @importFrom utils "read.table"
#' @examples
#' Produc
#' use(Produc)
#' savedata("Produc.csv")
#' listif()
#' dropvar(".*")
#' @export
use <- function (x,clear=FALSE, ...)
{
  if (exists("data", envir=data.env) & !clear)
  {
    if (eval(substitute({ is.data.frame(data) }), envir=data.env))
    {
      stop("data already exists in memory, will not delete (give option clear=TRUE to overwrite).")
    }
  }

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
    eval(substitute({ data <- read.csv(x,...) }), envir=data.env)

  if (type=="dta")
    eval(substitute({ tryCatch({ data <-  read.dta(x,...)},
                               error=function(e) data <- read.dta13(x,...))}),
         envir=data.env)
  if (type=="txt")
    eval(substitute({ data <- read.table(x,...) }),
         envir=data.env)

  if (type=="tab")
    eval(substitute({data <- read.csv(x,sep="\t",...) }),
         envir=data.env)

  if (!exists("data", envir=data.env))
    stop("Could not determine type of data. Did not load data.")
  else
    postuse()
}

#' @export
use.function <- function (x, ...)
{
  eval(substitute({data <- x(...) }), envir=data.env)
  postuse()
}

postuse <- function()
{
  eval(substitute({
    data$rownum <- 1:nrow(data)
  }),
  envir=data.env)
}

stringify <- function()
{
  eval(substitute({
    for (i in 1:ncol(data))
    {
      if (is.factor(data[,i]))
      {
        data[,i] <- as.character(data[,i])
      }
    }
  }), envir=data.env)
}

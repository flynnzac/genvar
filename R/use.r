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


#' uses a dataset, marking it as the active dataset
#'
#' @param x usually either a data.frame or a csv/dta filename to be imported.  An R function which returns a data.frame can also be specified.
#' @param clear if TRUE, erase current data if it already exists. If FALSE, back up data so that it can be switched to later via \code{switchdata} (default: FALSE).
#' @param type either "csv" or "dta" for loading csv or dta data set
#' @param ... other options to pass to \code{read.csv} in case x is a csv file or to \code{read.dta} or \code{read.dta13} depending on the type of file being loaded
#' @importFrom tools "file_ext"
#' @importFrom readstata13 "read.dta13"
#' @importFrom foreign "read.dta"
#' @importFrom utils "read.csv"
#' @importFrom utils "read.table"
#' @return returns NULL invisibly
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' listif()
#' dropvar(".*")
#' @export
use <- function (x,clear=FALSE, type=NULL, ...)
{
  if (eval(substitute({ is.data.frame(data) }), envir=data.env) && !clear)
  {
    cat("NOTE: Data already exists in memory, will not delete.  You can switch back to the dataset with switchdata using name 'last'.\n")
    other.data$last <- get("data", envir=data.env)
  }

  UseMethod("use", x)


  invisible(NULL)
}


#' @export
use.data.frame <- function (x,...)
{
  assign("data", x, envir=data.env)
  postuse()
}

#' @export
use.character <- function (x, clear=TRUE, type=NULL, ...)
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
  assert_loaded()
  eval(substitute({
    data$rownum <- 1:nrow(data)
  }),
  envir=data.env)
}

stringify <- function()
{
  assert_loaded()
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

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


#' preserve a data set before modification
#' @param data a data set to preserve
#' @return a value that can be passed to \code{restore} to restore the data set later
#' @examples
#' require(stats)
#' use(cars, clear=TRUE)
#' p <- preserve()
#' collapse("mean(dist)", "speed")
#' list()
#' restore(p, replace=TRUE)
#' list()
#' @export
preserve <- function (data=NULL)
{
  assert_loaded()
  if (is.null(data))
    data <- get("data", envir=data.env)

  envir <- new.env()
  assign("data", data, envir=envir)

  return(envir)
}

#' restore a dataset from a previous preserve to be currently used
#' @param envir a previous preserve value.
#' @param replace if TRUE, restore even if another dataset is in memory.  If FALSE, do not. 
#' @return the preserved data set
#' @examples
#' require(stats)
#' use(cars, clear=TRUE)
#' p <- preserve()
#' collapse("mean(dist)","speed")
#' list()
#' restore(p, replace=TRUE)
#' list()
#' @export
restore <- function (envir, replace=FALSE)
{
  if (is_loaded() && !replace)
  {
    stop("Dataset already in memory. Use replace=TRUE to overwrite.")
  }
  assign("data", get("data", envir=envir), envir=data.env)
}


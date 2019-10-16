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

#' get first few observations
#'
#' @param num how many of the first observations to get
#' @return returns the first \code{num} rows of data
#' @examples
#' use(cars, clear=TRUE)
#' headdata(5)
#'@export
headdata <- function (num)
{
  assert_loaded()
  listif(paste0("rownum <= ", num))
}

#' get last few observations
#'
#' @param num how many of the last few observations to get
#' @return returns last \code{num} rows of data
#' @examples
#' use(cars, clear=TRUE)
#' taildata(5)
#'@export
taildata <- function (num)
{
  assert_loaded()
  maxr <- eval(substitute({nrow(data)}), envir=data.env)
  listif(paste0("rownum > ", maxr-num))
}

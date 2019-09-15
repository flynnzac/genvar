## This file is part of arata.

## arata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## arata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with arata.  If not, see <https://www.gnu.org/licenses/>.

#' get first few observations
#'
#' @param num how many of the first observations to get
#'@export
headdata <- function (num)
{
  listif(paste0("rownum <= ", num))
}

#' get last few observations
#'
#' @param num how many of the last few observations to get
#'@export
taildata <- function (num)
{
  maxr <- eval(substitute({nrow(data)}), envir=data.env)
  listif(paste0("rownum > ", maxr-num))
}

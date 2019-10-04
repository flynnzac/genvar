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

#' a command to determine whether data is loaded
#'
#' @return returns TRUE if dataset is loaded in genvar and FALSE otherwise
#' @export
is_loaded <- function ()
{
  eval(substitute({is.data.frame(data)}), envir=data.env)
}

#' assert a dataset is loaded in genvar and error otherwise
#' @return returns NULL, invisibly
#' @export
assert_loaded <- function ()
{
  if (!is_loaded())
  {
    stop("Dataset not loaded.")
  }
  invisible(NULL)
}


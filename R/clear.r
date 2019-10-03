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

#' clears the dataset in memory
#'
#' removes a dataset from memory, errors if no dataset is loaded
#' 
#' @return returns NULL invisibly
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' clear()
#' listif()
#' @export
clear <- function ()
{
  assert_loaded()
  rm("data", envir=data.env)
  data.env <- new.env()

  invisible(NULL)
}


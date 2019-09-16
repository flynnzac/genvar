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


#' renames variables in the dataset
#'
#' @param var the name of the variable to rename
#' @param newvar the new name of the variable
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' rename("speed","velocity")
#' listif()
#' @export
rename <- function (var, newvar)
{
  eval(substitute({
    names(data)[names(data)==var] <- newvar
  }), envir=data.env)
}



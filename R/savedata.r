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

#' saves data to a CSV file
#'
#' @param file a file name to save the current data to
#' @examples
#' use(Produc, clear=TRUE)
#' savedata("Produc.csv")
#' @export
savedata <- function (file)
{
  eval(substitute({ write.csv(data, file=file, row.names=FALSE) }),
       envir=data.env)
}


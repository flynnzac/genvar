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

#' saves data to a CSV or RDS file
#'
#' @param file a file name to save the current data to
#' @param rds whether to save the file to an RDS file (default: FALSE)
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' savedata(file.path(tempdir(), "cars.csv"))
#' savedata(file.path(tempdir(), "cars.rds"), rds=TRUE)
#' @export
savedata <- function (file, rds=FALSE)
{
  assert_loaded()
  if (rds)
    eval(substitute({ saveRDS(data, file=file) }),
         envir=data.env)
  else
    eval(substitute({ write.csv(data, file=file, row.names=FALSE) }),
         envir=data.env)

  invisible(NULL)
}


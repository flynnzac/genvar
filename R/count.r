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



#' Counts how many observations (optionally, satisfying a condition)
#' @param ifstmt an optional argument which gives an condition that must be met for the observation to be counted 
#' @return returns the count 
#' @examples
#' use(cars, clear=TRUE)
#' count()
#' count(speed <= 20)
#' @export
count <- function (ifstmt)
{
  assert_loaded()

  if (missing(ifstmt))
  {
    eval(substitute({nrow(data)}), envir=data.env)
  } else {
    ifstmt <- gvcharexpr(enquo(ifstmt))
    eval(substitute({
      with(data, sum(eval(parse(text=ifstmt))))}),
      envir=data.env)
  }

}


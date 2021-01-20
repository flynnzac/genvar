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

#' check if panel is balanced
#'
#' indicates whether panel is balanced
#' @importFrom "plm" is.pbalanced
#' @export

xtbalanced <- function ()
{
  assert_loaded()
  eval(substitute({
    bal <- is.pbalanced(data)
    if (bal)
    {
      cat("Panel is balanced.\n")
    } else {
      cat("Panel is not balanced.\n")
    }
    bal
  }), envir=data.env)
}
  

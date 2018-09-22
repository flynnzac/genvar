
## This file is part of rtata.

## rtata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## rtata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with rtata.  If not, see <https://www.gnu.org/licenses/>.

#' Executes code on the dataset
#'
#' Executes an R expression using variables from the dataset.
#' @param expr an R expression which can use any of the variable names in the current dataset
#' @examples
#' use(cars)
#' do({coef(lm(speed~dist))})
#' @export
do <- function(expr)
{
  eval(substitute({with(data, expr)}),
       envir=data.env)
}

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

#' gets fitted values from a genvar regression object
#'
#' Gets fitted values from a genvar regression object.  For panel models, this predicts the non-fixed effects part of the regression.
#'
#'
#' Operates on the loaded estimation object, see \code{estimates_use}.
#' @return returns predictions from model
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' reg("dist", "speed")
#' gen("fit", "pred()")
#' listif()
#' @export
pred <- function ()
{
  assert_loaded()
  eval(substitute({
    model <- model.frame(last_estimates$rhs, data=data, na.action=NULL)
    matrix <- model.matrix(last_estimates$rhs, data=data)
    if (ncol(matrix) == (length(last_estimates$b)+1))
    {
      matrix <- matrix[,-1]
    }
    last_estimates$f(matrix %*% as.matrix(last_estimates$b))
  }), envir=data.env)
}


#' gets fitted values from a rata regression object
#'
#' Gets fitted values from a rata regression object.  For panel models, this predicts the non-fixed effects part of the regression.
#'
#'
#' Operates on the loaded estimation object, see \code{estimates_use}.
#' @examples
#' use(cars)
#' listif()
#' reg("dist", "speed")
#' gen("fit", "pred()")
#' listif()
#' @export
pred <- function ()
{
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


#' gets fitted values from a rata regression object
#'
#' Gets fitted values from a rata regression object.  This function
#' fixes a major annonyance with R's standard methods for recovering
#' fitted values: they do not handle missing values and confusingly
#' just omit them so that fitted[1] may not correspond to observation
#' 1.  With rata's fitted functions, you get NA's where NA's should
#' be.
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

    model <- model.frame(last_estimates$x, data=data, na.action=NULL)
    model %*% last_estimates$b
  }), envir=data.env)
}


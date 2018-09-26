#' captures an expression, returning 1 if there was an error and zero otherwise
#'
#' @param expr an expression to be evaluated
#' @param silent if TRUE,  suppress error messages from printing (default: FALSE)
#' @return 0 if the expression successfully ran and 1 otherwise
#' @examples
#' capture({log(1)})
#' capture({log(-1)})
#' capture({log(x)}) # where x is not an already-created variable
#' @export
capture <- function (expr, silent=FALSE)
{
  val <- try(expr,silent=silent)
  return (ifelse(class(val)=="try-error", 1, 0))
}


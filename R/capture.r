#' captures an expression, returning 1 if there was an error and zero otherwise
#'
#' @param expr an expression to be evaluated
#' @param silent if TRUE,  suppress error messages from printing (default: FALSE)
#' @return 0 if the expression successfully ran and 1 otherwise
#' @export
capture <- function (expr, silent=FALSE)
{
  val <- try(expr,silent=silent)
  return (ifelse(class(val)=="try-error", 1, 0))
}    
  

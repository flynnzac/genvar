#' @export
forvar <- function (varlist, action,...)
{
  varlist <- as.Formula(varlist)
  varlist <- attr(terms(formula(varlist,lhs=0,rhs=1)), "term.labels")

  for (var in varlist)
  {
    action(var,...)
  }
}

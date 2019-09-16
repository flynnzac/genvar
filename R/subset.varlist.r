#' generate a varlist that is a subset of another
#'
#' @param x a varlist
#' @param vars a set of variable names
#' @param ... currently ignored
#' @export
subset.varlist <- function(x, vars, ...)
{
  names <- x[x %in% vars]
  attr(names, "type") <- attr(x,"type")[x %in% vars]
  return (names)
}

#' Execute code in the datasets environment for all values of a vector, replacing a macro with the value in each iteration
#'
#' @param values the vector of values to loop over.  For example, specifying 1:5 would loop over integers from 1 to 5.
#' @param expr a quoted expression to evaluate in the loop which (presumably) uses the macro expression
#' @param macro a word to replace in the quoted expression with the values we are looping over (default: "%val")
#' @examples
#' use(cars)
#' listif()
#' forval (2:4, "gen('speed%val', 'speed^%val')")
#' listif()
#' @export
forval <- function (values, expr, macro="%val")
{
  for (val in values)
  {
    expr.sub <- gsub(macro, val, expr)
    eval(parse(text=expr.sub),
         envir=data.env)
  }
}

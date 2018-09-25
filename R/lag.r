#' a function to take lags and leads with panel data
#'
#' a function to take lags and leads with panel data, mostly a wrapper for \code{plm}'s lag function.
#' @param x variable to lag
#' @param k how many lags to take?  If a negative number, leads will be generated.
#' @examples
#' use(Produc)
#' xtset("year", "state")
#' gen("Lemp", "L(emp)")
#' gen("L2emp", "L(emp,2)")
#' headdata(10)
#' @export
L <- function (x,k=1,...)
{
  varname <- deparse(substitute(x))
  if ("index" %in% eval(substitute(names(attributes(data)))))
  {
    return (lag(data[,varname],k,...))
  } else {
    lag(x,k,...)
  }
}

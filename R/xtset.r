
#' prepares a panel or time series dataset for lag operations
#'
#' @param timevar the name of the variable to for the time dimension
#' @param obsvar the name of the variable to use for the observation dimension
#' @examples
#' use(Produc)
#' xtset("year", "state")
#' reg("emp", "unemp", effect="twoway")
#' reg("emp", "unemp", effect="obs")
#' reg("emp", "unemp", effect="time")
#' @export
xtset <- function (timevar=NULL, obsvar=NULL)
{
  eval(substitute({attr(data,"timevar") <- timevar}), envir=data.env)
  eval(substitute({attr(data, "obsvar") <- obsvar}), envir=data.env)
}


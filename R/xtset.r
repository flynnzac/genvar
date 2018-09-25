
#' prepares a panel dataset for lag operations
#'
#' prepares a panel dataset for lag operations.  The lag function in R is simply "lag(var,numlags)".  After calling \code{xtset}, this lag function will work on the panel in the way you would expect.
#' @param timevar the name of the variable to for the time dimension
#' @param obsvar the name of the variable to use for the observation dimension
#' @examples
#' use(Produc)
#' xtset("year", "state")
#' gen("Lemp", "lag(emp)")
#' listif(vars="emp Lemp")
#' reg("emp", "unemp", effect="twoway")
#' reg("emp", "unemp", effect="obs")
#' reg("emp", "unemp", effect="time")
#' @export
xtset <- function (timevar, obsvar)
{
  eval(substitute({data <- pdata.frame(data, index=c(obsvar,timevar))}))
  eval(substitute({attr(data,"timevar") <- timevar}), envir=data.env)
  eval(substitute({attr(data, "obsvar") <- obsvar}), envir=data.env)
}


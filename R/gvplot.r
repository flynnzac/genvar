#' convenience interface to R's \code{plot} command
#'
#' Executes a plot command in genvar's environment so that
#' gvplot(xvar,yvar) will plot a scatter plot of the variables xvar
#' and yvar in the genvar enviroment.
#'
#' @param ... arguments to be passed to \code{R}'s plot command.
#' @return returns NULL, invisibly
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' gen("laborforce", "emp/(1-unemp/100)")
#' empfrac = function (emp, laborforce) sum(emp)/sum(laborforce)
#' collapse("empfrac(emp,laborforce)", "year")
#' rename("empfrac(emp, laborforce)", "empfrac")
#' destring("year")
#' gvplot(year, empfrac, type="b", main="Employment Percentage over Time",
#' xlab="Year", ylab="Employment Percentage", pch=19)
#' @export
gvplot <- function (...)
{
  assert_loaded()

  eval(substitute({ with(data, plot(...)) }), envir=data.env)
  invisible(NULL)

}


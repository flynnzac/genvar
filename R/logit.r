#' estimate a logistic regression
#'
#' @param y name of the dependent variable
#' @param x names of the independent variables in varlist format, either "x1 x2 x3" or ~x1+x2+X3 format.
#' @param subset conditions to run the command only of a subset of the data (analogous to "if" statements in Stata)
#' @return b coefficient vector
#' @return V covariance matrix of coefficients
#' @export
logit <- function (y, x, subset=NULL)
{
}

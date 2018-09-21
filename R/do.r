
#' Executes code on the dataset
#'
#' Executes an R expression using variables from the dataset.
#' @param expr an R expression which can use any of the variable names in the current dataset
#' @export
do <- function(expr)
{
  eval(substitute({with(data, expr)}),
       envir=data.env)
}

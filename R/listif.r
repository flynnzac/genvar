#' @export
listif <- function (cond=NULL, ...)
{
  if (is.null(const))
    eval(substitute({data}), envir=data.env)
  else
    UseMethod("listif", cond)
}

#' @export
listif.character <- function (cond)
{
  eval(substitute({subset(data,with(data,eval(parse(text=cond))))}), envir=data.env)
}

#' @export
listif.formula <- function (cond, ifstmt=NULL)
{
  cond <- as.Formula(cond)
  vars <- attr(terms(formula(cond,lhs=0,rhs=1)), "term.labels")
  if (is.null(ifstmt))
    eval(substitute({data[,vars]}),envir=data.env)
  else
    eval(substitute({subset(data[,vars],with(data,eval(parse(text=ifstmt))))}), envir=data.env)
}

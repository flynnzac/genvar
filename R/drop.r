#' drops variables or rows from the dataset
#'
#' @param vars an optional argument with form, ~var1+var2+var3... Variables var1, var2, var3, ... are dropped.  This works like the Stata command: "drop var1 var2 var3 ...".
#' @param rows an optional argument which provides a condition that, if true, drops the row from the dataset.  For example, drop("female==1") would drop all women from the data set.
#'@export
#'
drop <- function (x)
{
  UseMethod("drop",x)
}

#'@export
drop.character <- function (x)
{

  eval(substitute({
    rows <- with(data, which(eval(parse(text=x))))
    data <- data[-rows,]}), envir=data.env)
}

#'@export
drop.formula <- function (x)
{
  form <- as.Formula(x)
  vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
  eval(substitute({data[,vars] <- NULL}),envir=data.env)
}

#' keeps some variables or rows in the dataset and drops the rest
#'
#' @param vars an optional argument with form, ~var1+var2+var3... Variables var1, var2, var3, ... are kept.  This works like the Stata command: "keep var1 var2 var3 ...".
#' @param rows an optional argument which provides a condition that, if true, keeps the row in the dataset (otherwise, the row is dropped).  For example, keep(data,rows="female==1") would keep all women in the data set and drop all men from it.
#'@export

keep <- function (x)
{
  UseMethod("keep",x)
}

#'@export
keep.character <- function (x)
{
  rows <- with(data, which(eval(parse(text=x))))
  eval(substitute({data <- data[rows,]}), envir=data.env)
}

#'@export
keep.formula <- function (x)
{
  form <- as.Formula(x)
  vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
  eval(substitute({data <- data[,vars]}),envir=data.env)
}

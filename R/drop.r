#' drops variables or rows from the dataset
#'
#' @param data a data frame to drop variables from.  On exit, the data frame will not have the variable or rows to drop.
#' @param vars an optional argument with form, ~var1+var2+var3... Variables var1, var2, var3, ... are dropped.  This works like the Stata command: "drop var1 var2 var3 ...".
#' @param rows an optional argument which provides a condition that, if true, drops the row from the dataset.  For example, drop(data,rows="female==1") would drop all women from the data set.
#'@export
drop <- function (data, vars=NULL, rows=NULL)
{
  if (is.null(vars) & is.null(rows))
    stop("At least one of the 'rows' or 'vars' options should be specified.")

  if (!is.null(vars) & !is.null(rows))
    stop("Can only drop variables or rows, not both")
  
  if (!is.null(vars))
    {
      form <- as.Formula(vars)
      vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
      eval.parent(substitute(data[,vars] <- NULL))
    } else {
      rows <- with(data, which(eval(parse(text=rows))))
      eval.parent(substitute(data <- data[-rows,]))
    }
}

#' keeps some variables or rows in the dataset and drops the rest
#'
#' @param data a data frame.  On exit, the data frame will not have the variable or rows that do not satisfy the keep conditions.
#' @param vars an optional argument with form, ~var1+var2+var3... Variables var1, var2, var3, ... are kept.  This works like the Stata command: "keep var1 var2 var3 ...".
#' @param rows an optional argument which provides a condition that, if true, keeps the row in the dataset (otherwise, the row is dropped).  For example, keep(data,rows="female==1") would keep all women in the data set and drop all men from it.
#'@export
keep <- function (data, vars=NULL, rows=NULL)
{
  
  if (is.null(vars) & is.null(rows))
    stop("At least one of the 'vars' or 'rows' options should be specified.")

  if (!is.null(vars) & !is.null(rows))
    stop("Can only keep variables or rows, not both")
  
  if (!is.null(vars))
    {
      form <- as.Formula(vars)
      vars <- attr(terms(formula(form,lhs=0,rhs=1)), "term.labels")
      eval.parent(substitute(data[,which(!is.element(names(data),vars))] <- NULL))
    } else {
      rows <- with(data, which(eval(parse(text=rows))))
      eval.parent(substitute(data <- data[rows,]))
    }
}

library(Formula)

#' collapses a data set by variables using arbitrary aggregation functions
#'
#' @param data a data set.  On exit, the dataset is the collapsed, aggregated dataset.
#' @param form an argument with the form ~fun1(var1)+fun2(var2)+fun3(var3)+...|byvar1+byvar2+... . where fun1, fun2, and fun3 are aggregation functions like "mean", "sum", "max", etc.  \code{data} will contain all unique levels of (byvar1,byvar2,...) and fun1(var1) conditional on each by level.  In Stata, this is equivalent to collapse (fun1) var1 (fun2) var2 (fun3) var3 ..., by(byvar1 byvar2)
#' @examples
#' require(stats)
#' data <- cars
#' print(data)
#' collapse(data,~mean(dist)|speed)
#' print(data)
#' @importFrom Formula "as.Formula"
#' @export
collapse <- function(data, form)
{
  form <- as.Formula(form)

  collapse.exp <- strsplit(as.character(formula(form,lhs=0,rhs=1))[-1],"\\+")[[1]]

  by.data <- model.frame(formula(form,lhs=0,rhs=2), data=data, na.action=NULL)

  int <- interaction(by.data)
  s <- split(data,int)
  res <- sapply(s, function (u)
    with (u, sapply(collapse.exp, function (expr) eval(parse(text=expr)))))
  res <- t(as.matrix(res))

  new.names <- collapse.exp
  for (n in 1:length(new.names))
  {
    by.data[,new.names[n]] <- res[n,match(as.character(int),names(s))]
  }

  eval.parent(substitute(data <- unique(by.data)))
}

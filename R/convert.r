#' turn a variable of another type into a string variable
#'
#' @param varlist variables to convert, either in the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @export
tostring <- function (varlist)
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }
  vars <- attr(terms(varlist), "term.labels")
  for (v in vars)
  {
    gen(v,paste("as.character(",v,")",sep=""),
        replace=TRUE)
  }
}

#' turn a variable with string type into a numeric value
#'
#' @param varlist variables to convert, either in the form "var1 var2 var3" or in the form ~var1+var2+var3.
#' @export
destring <- function (varlist)
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }

  vars <- attr(terms(varlist), "term.labels")
  desc <- subset(describe(), vars)
  for (v in 1:length(vars))
  {
    if (attr(desc,"type")[v] == "factor")
    {
      tostring(vars[v])
    }
    
    gen(vars[v],
        paste("as.numeric(", vars[v], ")",sep=""),
        replace=TRUE)
      
  }
      
}



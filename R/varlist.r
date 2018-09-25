
#' creates a formula object from a varlist, mostly for internal use.
#'
#' A varlist in \code{rata} is either a space-separated string with wildcard characters, "var1 var2 var3 x*", or an R formula object ~var1+var2+var3+x1+x2....  This function converts from the more user-friendly space-separated string format to the formula format.
#' @param x the varlist to be converted in "var1 var2 var3" format.  Can be specified using the \emph{globbing} characters "*" (match zero or more of any character) or "?" (match any single character) like "var*" or "var?" for "var1 var2 var3" or using regular expressions if \code{regex=TRUE} ("var[0-9]+" = "var1 var2 var3").
#' @return a formula object which can be passed to \code{model.frame}
#' @export
varlist <- function (x)
{
  n <- describe()
  x <- strsplit(x, " ")[[1]]

  x <- gsub("\\*", "\\.*", x)
  x <- gsub("\\?", "\\.", x)

  x <- sapply(x, function (u)
    ifelse(grepl("\\.",u), n[grepl(u,n)], u))

  x <- unique(unlist(x))
  as.formula(paste("~",paste0(x,collapse="+"),sep=""))
}


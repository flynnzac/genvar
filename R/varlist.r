
#' creates a formula object from a varlist, mostly for internal use.
#'
#' @param x the varlist to be converted in "var1 var2 var3" format.  Can be specified using the \emph{globbing} characters "*" (match zero or more of any character) or "?" (match any single character) like "var*" or "var?" for "var1 var2 var3" or using regular expressions if \code{regex=TRUE} ("var[0-9]+" = "var1 var2 var3").
#' @param regex if TRUE, evaluate each string in the varlist \code{x} as a regular expression.  By default, allow for the wildcard character * which matches zero or more of any character or ? which matches any single character (default: FALSE).
#' @return a formula object which can be passed to \code{model.frame}
#' @export
varlist <- function (x, regex=FALSE)
{
  n <- describe()
  x <- strsplit(x, " ")[[1]]
  if (!regex)
  {
    x <- gsub("\\*", "\\.*", x)
    x <- gsub("\\?", "\\.", x)
  }

  x <- as.vector(sapply(x, function (u) n[grepl(u,n)]))
  as.formula(paste("~",paste0(x,collapse="+"),sep=""))
}


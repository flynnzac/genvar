
#' creates a formula object from a varlist, mostly for internal use.
#'
#' @param x the varlist to be converted in "var1 var2 var3" format.  Will eventually work with globbing so that "var*" can be used to refer to all variables that begin with var
#' @return a formula object which can be passed to \code{model.frame}
#' @export
varlist <- function (x)
{
  n <- describe()
  x <- strsplit(x, " ")[[1]]
  x <- as.vector(sapply(x, function (u) n[grepl(u,n)]))
  as.formula(paste("~",paste0(x,collapse="+"),sep=""))
}


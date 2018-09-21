#' @export
describe <- function(pattern=NULL)
{
  names <- eval(substitute({attr(data,"names")}),envir=data.env)
  if (is.null(pattern))
    names
  else
    names[grepl(pattern,names)]
}

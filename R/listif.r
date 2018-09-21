#' @export
listif <- function (ifstmt=NULL)
{
  if (is.null(ifstmt))
    eval(substitute({data}), envir=data.env)
  else
    eval(substitute({subset(data,with(data,eval(parse(text=ifstmt))))}), envir=data.env)
}

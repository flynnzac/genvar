#' @export
count <- function (ifstmt=NULL)
{
  if (is.null(ifstmt))
    eval(substitute({nrow(data)}), envir=data.env)
  else
    eval(substitute({
      with(data, sum(eval(parse(text=x))))}),
      envir=data.env)
}


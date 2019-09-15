isloaded <- function ()
{
  eval(substitute({is.data.frame(data)}), envir=data.env)
}

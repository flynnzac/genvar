#' clears the dataset in memory
#'
#' @examples
#' use(cars, clear=TRUE)
#' listif()
#' clear()
#' listif()
#' @export
clear <- function ()
{
  if (isloaded())
    rm("data", envir=data.env)
}


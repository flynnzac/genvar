#' clears the dataset in memory
#'
#' @examples
#' use(cars)
#' listif()
#' clear()
#' listif()
#' @export
clear <- function ()
{
  rm("data", envir=data.env)
}


#' exports data frame from genvar environment to R environment
#'
#' Returns the data frame currently in the genvar environment.  It is
#' equivalent to calling \code{listif()}, but the name is not as
#' intuitive to use for this purpose.
#' @return the data frame currently in the genvar environment
#' @examples
#' use(cars, clear=TRUE)
#' getdata()
#' all(getdata() == listif())
#' @export
getdata <- function ()
{
  get("data", envir=data.env)
}


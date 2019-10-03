#' exports data frame from genvar environment to R environment
#'
#' @return the data frame currently in the genvar environment
#' @export
getdata <- function ()
{
  get("data", envir=data.env)
}


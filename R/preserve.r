#' preserve a data set before modification
#' @param data a data set to preserve
#' @return a value that can be passed to \code{restore} to restore the data set later
#' @examples
#' require(stats)
#' use(cars)
#' p <- preserve()
#' collapse(~mean(dist)|speed)
#' list()
#' restore(p)
#' list()
#' @export
preserve <- function (data=NULL)
{
  if (is.null(data))
    data <- get("data", envir=data.env)

  envir <- new.env()
  assign("data", data, envir=envir)

  return(envir)
}

#' restore a dataset from a previous preserve to be currently used
#' @param envir a previous preserve value.
#' @return the preserved data set
#' @examples
#' require(stats)
#' use(cars)
#' p <- preserve()
#' collapse(~mean(dist)|speed)
#' list()
#' restore(p)
#' list()
#' @export
restore <- function (envir)
{
  assign("data", get("data", envir=envir), envir=data.env)
}


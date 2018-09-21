#' preserve a data set before modification
#' @param data a data set to preserve
#' @return a value that can be passed to \code{restore} to restore the data set later
#' @examples
#' require(stats)
#' data <- cars
#' p <- preserve(data)
#' collapse(data,~mean(dist)|speed)
#' data <- restore(p)
#' @export
preserve <- function (data)
{
  envir <- new.env()
  assign("data", data, envir=envir)

  return(envir)
}

#' restore a dataset from a previous preserve
#' @param envir a previous preserve value.
#' @return the preserved data set
#' @examples
#' require(stats)
#' data <- cars
#' p <- preserve(data)
#' collapse(data,~mean(dist)|speed)
#' data <- restore(p)
#' @export
restore <- function (envir)
{
  return (get("data", envir=envir))
}


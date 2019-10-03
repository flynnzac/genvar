#' creates a dataset of a given number of observations
#'
#' Creates a dataset of a given number of observations.  Does so by creating a variable called "v1" with all missing values.
#' @param n the number of observations to make the new dataset
#' @param replace if TRUE, replace a dataset in memory, if FALSE, error if a dataset is already loaded
#' @return returns NULL, invisibly
#' @examples
#' builddata(100, replace=TRUE)
#' listif()
#' @export
builddata <- function (n, replace=FALSE)
{
  if (is_loaded() && !replace)
  {
    stop("Dataset already loaded into memory.")
  }

  eval(substitute({
    data <- data.frame(v1=rep(NA, times=n))
  }), envir=data.env)

  invisible(NULL)
}

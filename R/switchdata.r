#' switches data sets among the datasets you have in memory
#'
#' @param name Name of the dataset to switch to.  The name "last" is reserved for the last dataset loaded (default: last)
#' @param clear If TRUE, erase the currently loaded dataset.  If FALSE, make the current dataset the new "last". (default: FALSE)
#' @return returns NULL invisibly
#' @examples
#' use(cars, clear=TRUE)
#' namedata("cars", clear=TRUE)
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' listif()
#' switchdata("cars")
#' listif()
#' switchdata()
#' listif()
#' listdata()
#' @export
switchdata <- function (name="last", clear=FALSE)
{
  if (!exists(name, envir=other.data) ||
      !is.data.frame(get(name, envir=other.data)))
  {
    stop(paste0("Dataset ", name, "not found."))
  }
  use(get(name, envir=other.data), clear=clear)

}

#' names a data set so that it can be restored by that name later on
#'
#' @param name a string giving the desired name for the dataset
#' @param original a string giving the current name of the dataset or NULL to set the name for the currently-loaded dataset (default: NULL)
#' @param clear if TRUE, overwrite current dataset at that name, if FALSE, stop if the dataset already exists (default: FALSE).
#' @return returns NULL invisibly
#' @examples
#' use(cars, clear=TRUE)
#' namedata("cars", clear=TRUE)
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' namedata("product", clear=TRUE)
#' listdata()
#' namedata("cars_new_name", original="cars", clear=TRUE)
#' listdata()
#' @export
namedata <- function (name, original=NULL, clear=FALSE)
{
  if (is.null(original))
    assert_loaded()

  if (exists(name, other.data) & !clear)
    stop(paste0("Data named ", name, " already exists."))

  if (is.null(original))
    assign(name, value=get("data", envir=data.env), envir=other.data)
  else
    assign(name, value=get(original, envir=other.data), envir=other.data)

  invisible(NULL)
}

#' lists datasets currently in memory
#'
#' @return returns a data frame of the names of the datasets in memory and basic descriptions (number of observations, variables).
#' @examples
#' use(cars, clear=TRUE)
#' namedata("cars", clear=TRUE)
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' namedata("product", clear=TRUE)
#' listdata()
#' namedata("cars_new_name", original="cars", clear=TRUE)
#' listdata()
#' @export
listdata <- function ()
{
  name <- names(other.data)
  rows <- sapply(other.data, nrow)
  cols <- sapply(other.data, ncol)
  data.frame(name=name, observations=rows, variables=cols)
}



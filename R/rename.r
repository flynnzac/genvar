#' renames variables in the dataset
#' 
#' @param var the name of the variable to rename
#' @param newvar the new name of the variable
#' @examples
#' use(cars)
#' listif()
#' rename("speed","velocity")
#' listif()
#' @export
rename <- function (var, newvar)
{
  eval(substitute({
    names(data)[names(data)==var] <- newvar
  }), envir=data.env)
}




#' saves data to a CSV file
#'
#' @param file a file name to save the current data to
#' @examples
#' use(Produc)
#' savedata("Produc.csv")
#' @export
savedata <- function (file)
{
  eval(substitute({ write.csv(data, file=file, row.names=FALSE) }),
       envir=data.env)
}

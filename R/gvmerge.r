#' merge two datasets
#'
#' Merges two datasets using either a left (keep all elements in current datasets and replace with missing if not present in new set), right (keep all elements in new dataset), outer (keep all observations in both datasets), or inner join (only keep elements in both datasets)
#' @param data dataset to merge in, either an R data frame, a csv file name, or a dta (Stata) file name.
#' @param on a variable list to merge on with the form "var1 var2 var3" (or possibly unquoted if a single variable).
#' @param kind one of "left", "right", "outer", or "inner" (default: "left")
#' @param ... extra options to pass to \code{read.csv} or \code{read.dta} (for old Stata files) or \code{read.dta13} (for newer ones).
#' @return returns NULL, invisibly
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' collapse(mean(emp), year)
#' rename(mean(emp), avgemp)
#' gvmerge(Produc, on="year", kind="right")
#' listif()
#' @export
gvmerge <- function (data, on, kind="left", ...)
{
  UseMethod("gvmerge", data)
}

#' @export
gvmerge.data.frame <- function (data, on, kind="left", ...)
{

  on <- gvcharexpr(enquo(on))
  m <- merge(get("data", envir=data.env), data,
             by=strsplit(on, "\\s+", perl=TRUE)[[1]],
             all.x=(kind=="left" | kind=="outer"),
             all.y=(kind=="right" | kind=="outer"))

  use(m, clear=TRUE)
  invisible(NULL)
}

#' @export
gvmerge.character <- function (data, on, kind="left", ...)
{
  type <- file_ext(data)

  on <- gvcharexpr(enquo(on))
  if (type=="csv")
  {
    data <- read.csv(data, ...)
  } else if (type=="dta") {
    data <- tryCatch({ read.dta(data, ...) },
                     error=function (e)
                       read.dta13(data, ...))
  } else {
    stop("File type must be either CSV or DTA (Stata).")
  }

  gvmerge(data, on, kind)
}


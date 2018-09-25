#' get first few observations
#'
#' @param num how many of the first observations to get
#'@export
headdata <- function (num)
{
  listif(paste0("rownum <= ", num))
}

#' get last few observations
#'
#' @param num how many of the last few observations to get
#'@export
taildata <- function (num)
{
  maxr <- eval(substitute({nrow(data)}), envir=data.env)
  listif(paste0("rownum > ", maxr-num))
}

#' add observations to the data set
#'
#' Add observations to the data set, similar in functionality to Stata's append command
#' @param obs one of three possible input types:
#'\itemize{
#' \item An R data frame with the same columns as the current dataset.
#' \item A comma-separated string in the following format: "var1=1,var2=2,var3=3" which inputs a single observation.
#' \item An integer in which case \code{obs} entirely missing observation are added to the dataset
#' }
#' @export
addobs <- function (obs)
{
  UseMethod("addobs",obs)
}

#' @export
addobs.character <- function(obs)
{
  lst <- eval(parse(text=paste0("list(",obs,")")))
  lst$rownum <- eval(substitute({max(data[,"rownum"])}),
                     envir=data.env)+1
  eval(substitute({
    data <- rbind(data, lst)
  }),envir=data.env)
  postuse()
}

#' @export
addobs.data.frame <- function(obs)
{
  eval(substitute({
    obs$rownum <- 1
    data <- rbind(data,obs)
  }), envir=data.env)
  postuse()
}

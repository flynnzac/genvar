## This file is part of arata.

## arata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## arata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with arata.  If not, see <https://www.gnu.org/licenses/>.

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
  stringify()
  lst <- eval(parse(text=paste0("list(",obs,")")))
  lst$rownum <- eval(substitute({max(data[,"rownum"])}),
                     envir=data.env)+1
  eval(substitute({
    ispanel <-"timevar" %in% names(attributes(data))
    if (ispanel)
    {
      timevar <- attr(data,"timevar")
      obsvar <- attr(data,"obsvar")
    }
    data <- rbind(data, lst)
    if (ispanel)
    {
      xtset(timevar,obsvar)
    }
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

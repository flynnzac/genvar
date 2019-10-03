## This file is part of genvar.

## genvar is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## genvar is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with genvar.  If not, see <https://www.gnu.org/licenses/>.


#' store \code{genvar} estimates
#'
#' @param name name to use to store current estimates from a \code{genvar} estimation function like \code{reg}, \code{logit}, or \code{probit}.
#' @return returns NULL, invisibly
#' @export
estimates_store <- function (name)
{
  eval(substitute({ assign(name,last_estimates) }),
       envir=data.env)

  invisible(NULL)
}

#' save \code{genvar} estimates
#'
#' @param file file to save current estimates to.
#' @return returns NULL, invisibly
#' @export
estimates_save <- function (file)
{
  save("last_estimates", file=file, envir=data.env)
  invisible(NULL)
}

#' restore \code{genvar} estimates
#'
#' @param name name of estimates to be restored
#' @return returns NULL, invisibly
#' @export
estimates_restore <- function (name)
{
  eval(substitute({
    last_estimates <- get(name)
  }), envir=data.env)

  invisible(NULL)
}

#' loads \code{genvar} estimates from file
#'
#' @param file file to load estimates from.
#' @return returns NULL, invisibly
#' @export
estimates_use <- function (file)
{
  load(file, envir=data.env)

}

#' display estimation results
#'
#' @param name name of estimates to be replaced.  If unspecified, print current estimates.
#' @export
estimates_print <- function (name=NULL)
{
  if (is.null(name))
    name <- "last_estimates"

  get(name, envir=data.env)
}


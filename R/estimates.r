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
#' @examples
#' use(cars, clear=TRUE)
#' reg(speed,dist)
#' estimates_store("speed_dist")
#' reg(dist,speed)
#' estimates_store("dist_speed")
#' estimates_get("speed_dist")
#' estimates_get("dist_speed")
#' @export
estimates_store <- function (name)
{
  eval(substitute({ assign(name,last_estimates) }),
       envir=data.env)

  invisible(NULL)
}

#' save \code{genvar} estimates
#'
#' @param estfile file to save current estimates to.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' reg(speed,dist)
#' fp <- file.path(tempdir(), "myest.rdata")
#' estimates_save(fp)
#' clear()
#' estimates_use(fp)
#' estimates_get()
#' @export
estimates_save <- function (estfile)
{

  eval(substitute(save(last_estimates, file=estfile)), envir=data.env)
  invisible(NULL)
}

#' restore \code{genvar} estimates
#'
#' @param name name of estimates to be restored
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' reg(speed, dist)
#' estimates_store("speed_dist")
#' reg(dist,speed)
#' estimates_get()
#' estimates_restore("speed_dist")
#' estimates_get()
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
#' @examples
#' use(cars, clear=TRUE)
#' reg(speed,dist)
#' fp <- file.path(tempdir(), "myest.rdata")
#' estimates_save(fp)
#' clear()
#' estimates_use(fp)
#' estimates_get()
#' @export
estimates_use <- function (file)
{
  load(file, envir=data.env)

}

#' display estimation results
#'
#' @param name name of estimates to be returned.  If unspecified, return current estimates.
#' @return returns a table of the estimated coefficients and standard errors
#' @examples
#' use(cars, clear=TRUE)
#' reg(speed,dist)
#' estimates_store("speed_dist")
#' reg(dist,speed)
#' estimates_store("dist_speed")
#' estimates_get("speed_dist")
#' estimates_get("dist_speed")
#' @export
estimates_get <- function (name=NULL)
{
  if (is.null(name))
    name <- "last_estimates"

  get(name, envir=data.env)
}


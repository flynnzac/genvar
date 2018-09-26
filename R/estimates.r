#' store \code{rata} estimates
#'
#' @param name name to use to store current estimates from a \code{rata} estimation function like \code{reg}, \code{logit}, or \code{probit}.
#' @export
estimates_store <- function (name)
{
  eval(substitute({ assign(name,last_estimates) }),
       envir=data.env)
}

#' save \code{rata} estimates
#'
#' @param file file to save current estimates to.
#' @export
estimates_save <- function (file)
{
  save("last_estimates", file=file, envir=data.env)
}

#' restore \code{rata} estimates
#'
#' @param name name of estimates to be restored
#' @export
estimates_restore <- function (name)
{
  eval(substitute({
    last_estimates <- get(name)
  }), envir=data.env)
}

#' loads \code{rata} estimates from file
#'
#' @param file file to load estimates from.
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


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


#' estimate a logistic regression
#'
#' @param y name of the dependent variable
#' @param x names of the independent variables in varlist format, either "x1 x2 x3" or ~x1+x2+X3 format.
#' @param subset conditions to run the command only of a subset of the data (analogous to "if" statements in Stata)
#' @param weights the name of a variable to use for weights in estimation
#' @param linkfunc specify the linking function (logit, by default).  Can set to "probit" to do probit estimation or use \code{probit} (which is equivalent).
#' @param ... other options to pass to \code{glm}
#' @return b coefficient vector
#' @return V covariance matrix of coefficients
#' @export
logit <- function (y, x, subset=NULL, weights=NULL, linkfunc="logit", ...)
{
  assert_loaded()
  
  if (!inherits(x, "formula"))
  {
    x <- varlist(x)
  }

  form <- as.formula(paste0(y, paste0(x,collapse="")))

  eval(substitute({
    if (is.null(weights))
    {
      model <- glm(form, family=binomial(link=linkfunc), data=data, ...)
    } else {
      model <- glm(form, family=binomial(link=linkfunc), data=data,
                   weights=data[,weights], ...)
    }
    
    last_estimates <- list(b=coef(model), V=vcov(model),
                           f=ifelse(linkfunc=="logit",
                                    function (u) 1/(1+exp(-1*u)),
                                    function (u) pnorm(u)),
                           model=model)
    class(last_estimates) <- "genvar_est"
    last_estimates
  }), envir=data.env)
}

#' estimate a probit regression
#'
#' \code{probit(...)} is equivalent to \code{logit(..., linkfunc="probit")}.
#' @param ... options to pass to \code{logit}
#' @export
probit <- function (...)
  logit(..., linkfunc="probit")

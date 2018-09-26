## This file is part of rata.

## rata is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, under version 3 of the License.

## rata is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with rata.  If not, see <https://www.gnu.org/licenses/>.


library(sandwich)
library(clubSandwich)
#' regress y on x with robust standard errors, clustered standard errors, HAC standard errors, panel fixed effects, etc
#'
#' @param y name of the dependent variable
#' @param x names of the independent variables in either "x1 x2 x3" format or ~x1+x2+x3 format.
#' @param subset conditions to subset the data
#' @param effect either "twoway", "obs", or "time" for fixed effects,
#' @param robust whether to use robust standard errors
#' @param hac which variable to order by to compute heteroskedastic and auto correlation standard errors (if unspecified, do not do HAC correction)
#' @param cluster a variable list giving the names of the variables to cluster by in producing clustered standard errors
#' @param rtype gives the type of heteroskedasticity correction to make.  By default, it is "1" to implement HC1 which is the same as Stata's small sample corrected standard errors.  rtype can be any integer from 0 to 3 with each value corresponding to a different heteroskedastic correction (HCx).  See documention for \code{vcovHC} in package \code{sandwich}.
#' @return b coefficient vector
#' @return V covariance matrix of coefficients
#' @importFrom sandwich "vcovHC"
#' @importFrom clubSandwich "vcovCR"
#' @export
reg <- function (y, x, subset=NULL, effect=NULL, robust=TRUE, hac=NULL,cluster=NULL,rtype=1)
{
  if (!inherits(x, "formula"))
  {
    x <- varlist(x)
  }

  form <- as.formula(paste0(y, paste0(x,collapse="")))

  if (robust & is.null(hac) & is.null(cluster))
  {
    covtype <- "robust"
  }

  if (!is.null(cluster))
  {
    covtype <- "cluster"
    if (!inherits(cluster,"formula"))
    {
      cluster <- varlist(cluster)
    }

    cluster <- attr(terms(cluster),"term.labels")
  }

  if (!is.null(hac))
  {
    covtype <- "hac"
  }

  if (!robust)
  {
    covtype <- "homoskedastic"
  }

  if (is.null(effect))
  {
    eval(substitute({model <- lm(form,data=data,subset=subset)}),
         envir=data.env)

    if (covtype=="robust")
    {
      eval(substitute({v <- vcovHC(model,type=paste0("HC",rtype))}),
           envir=data.env)
    }

    if (covtype=="hac")
    {
      eval(substitute({v <- vcovHAC(model,
                                    order.by=data[,hac],
                                    data=data)}),
           envir=data.env)
    }
    if (covtype=="cluster")
    {
      eval(substitute({v <- vcovCR(model,
                                   cluster=interaction(data[,cluster]),
                                   type=paste0("CR",rtype))}),
           envir=data.env)
    }

    if (covtype=="homoskedastic")
    {
      eval(substitute({ v <- vcov(model) }),
           envir=data.env)
    }

    eval(substitute({
      last_estimates <- list(b=model$coef, V=v)
      class(last_estimates) <- "rata_est"
      last_estimates
    }), envir=data.env)
  }
}

#' @export
print.rata_est<- function (x,...)
{
  print(data.frame(coef=x$b, stddev=sqrt(diag(x$V))))
}

#' @export
coef.rata_est<- function (object,...)
{
  object$b
}

#' @export
vcov.rata_est<- function (object,...)
{
  object$V
}


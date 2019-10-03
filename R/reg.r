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

#' regress y on x with robust standard errors, clustered standard errors, HAC standard errors, panel fixed effects, etc
#'
#' regress y on x with robust standard errors, clustered standard errors, HAC standard errors, panel fixed effects, etc.
#' @param y name of the dependent variable
#' @param x names of the independent variables in either "x1 x2 x3" format or ~x1+x2+x3 format. To include a variable as a categorical variable (when you would use "i.state" to get state dummies in Stata), include it as "factor(state)".
#' @param subset conditions to subset the data
#' @param effect either "twoways", "individual", or "time" for fixed effects.  Dataset must already have been \code{xtset}.
#' @param robust whether to use robust standard errors
#' @param hac which variable to order by to compute heteroskedastic and auto correlation standard errors (if unspecified, do not do HAC correction)
#' @param cluster a variable list giving the names of the variables to cluster by in producing clustered standard errors
#' @param rtype gives the type of heteroskedasticity correction to make.  By default, it is "1" to implement HC1 which is the same as Stata's small sample corrected standard errors.  rtype can be any integer from 0 to 3 with each value corresponding to a different heteroskedastic correction (HCx).  See documention for \code{vcovHC} in package \code{sandwich}.
#' @return b coefficient vector
#' @return V covariance matrix of coefficients
#' @importFrom sandwich "vcovHC"
#' @importFrom sandwich "vcovHAC"
#' @importFrom clubSandwich "vcovCR"
#' @importFrom plm "plm"
#' @export
reg <- function (y, x, subset=NULL, effect=NULL, robust=TRUE, hac=NULL,cluster=NULL,rtype=1)
{
  assert_loaded()
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
  } else {
    eval(substitute({model <- plm(form,data=data,subset=subset,model="within", effect=effect)}),
         envir=data.env)
  }

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

    if (is.null(effect))
    {
      eval(substitute({v <- vcovCR(model,
                                   interaction(data[,cluster]),
                                   paste0("CR",rtype))}),
           envir=data.env)
    } else {
      ## there is an issue in vcovCR with evaluating the function in
      ## another environment if a panel estimate so pull into the
      ## environment (it can't find the dataset).

      data <- get("data", envir=data.env)
      assign("v", vcovCR(get("model", envir=data.env),
                         interaction(data[,cluster]),
                         type=paste0("CR",rtype)),
             envir=data.env)
    }

  }

  if (covtype=="homoskedastic")
  {
    eval(substitute({ v <- vcov(model) }),
         envir=data.env)
  }

  eval(substitute({
    last_estimates <- list(b=model$coef, V=v, f=I, rhs=x, model=model)
    class(last_estimates) <- "genvar_est"
    last_estimates
  }), envir=data.env)
}


#' @export
print.genvar_est<- function (x,...)
{
  print(data.frame(coef=x$b, stddev=sqrt(diag(x$V))))
}

#' @export
coef.genvar_est<- function (object,...)
{
  object$b
}

#' @export
vcov.genvar_est<- function (object,...)
{
  object$V
}


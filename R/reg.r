library(sandwich)

#' regress y on x with robust standard errors, clustered standard errors, HAC standard errors, panel fixed effects, etc
#'
#' @param y name of the dependent variable
#' @param x names of the independent variables in either "x1 x2 x3" format or ~x1+x2+x3 format.
#' @param subset conditions to subset the data
#' @param effect either "twoway", "obs", or "time" for fixed effects,
#' @param robust whether to use robust standard errors
#' @param hac which kernel to use for heteroskedastic and auto correlation standard errors
#' @param cluster the name of the variable to use for clustered standard errors
#' @return b coefficient vector
#' @return V covariance matrix of coefficients
#' @importFrom sandwich "vcovHC"
#' @export
reg <- function (y, x, subset=NULL, effect=NULL, robust=TRUE, hac=NULL,cluster=NULL)
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
      eval(substitute({v <- vcovHC(model)}),
           envir=data.env)
    }

    if (covtype=="hac")
    {
      eval(substitute({v <- vcovHAC(model,
                                    order.by=varlist(attr(data,"timevar")),data=data)}),
           envir=data.env)
    }
    ret <- eval(substitute({list(b=model$coef, V=v)}), envir=data.env)
    class(ret) <- "reg"
    return (ret)
    
  }
}

#' @export
print.reg <- function (x,...)
{
  print(data.frame(coef=x$b, stddev=sqrt(diag(x$V))))
}

#' @export
coef.reg <- function (object,...)
{
  object$b
}

#' @export
vcov.reg <- function (object,...)
{
  object$V
}


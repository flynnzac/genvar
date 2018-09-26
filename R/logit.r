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
  if (!inherits(x, "formula"))
  {
    x <- varlist(x)
  }

  form <- as.formula(paste0(y, paste0(x,collapse="")))

  eval(substitute({
    model <- glm(form, family=binomial(link=link), data=data,
                 weights=ifelse(is.null(weights), NULL,
                                data[,weights]),...)
    last_estimates <- list(b=coef(model), V=vcov(model))
    class(last_estimates) <- "rata_est"
    last_estimates
  }), envir=data.env)
}

#' estimate a probit regression
#'
#' \code{probit(...)} is equivalent to \code{logit(..., linkfunc="probit")}.
#' @export
probit <- function (...)
  logit(..., linkfunc="probit")

#' generates a new variable that is a transformation of existing variables in the dataset or replaces one
#'
#' @param data a data set.  On exit, the dataset contains a newly generated variable or an existing variable is replaced.
#' @param form this argument gives the name of the variable to generate, the variables that are used to create the new variable, and an option to generate the variable by a certain set of variables in the following format: newvar~var1+var2+var3|byvar1+byvar2.
#' @param fun a function of the variables provided above, the value of which is put in newvar.  For example, if newvar~var1+var2+var3|byvar1+byvar2 were specified and fun=function() sum(var1+var2+var3), then newvar would be the sum of var1, var2, and var3 for each level of (byvar1, byvar2).  In Stata, the same command would be: "by byvar1 byvar2: gen newvar = sum(var1+var2+var3)".
#' @param subset only generate values if the condition provided in subset is true.  Make sure to enclose the expression in quotes, like so: subset="female==1 & highschool==1" to generate the values only for women who graduated from highschool.  This option is used like the "if" in Stata.
#' @param replace either TRUE or FALSE.  If FALSE (default), the code refuses to alter the variable if the variable already exists.  Otherwise, if replace=TRUE, then the values will be replaced.
#' @importFrom stats "formula"
#' @importFrom stats "model.frame"
#' @importFrom stats "terms"
#' @export
gen <- function (data, form, fun, subset=NULL, replace=FALSE)
{
  form <- as.Formula(form)
  repvar <- as.character(formula(form,lhs=1,rhs=0))[2]
  if (replace==FALSE && repvar %in% names(data))
  {
    stop(paste("replace=FALSE and variable ", repvar, " already in data. Call with option replace=TRUE to replace data.",sep=""))
  }

  if (is.null(subset))
  {
    subset <- rep(TRUE, times=nrow(data))
  } else {
    subset <- with(data, eval(parse(text=subset)))
  }

  frame <- model.frame(formula(form,lhs=0,rhs=1), data=subset(data,subset), na.action=NULL)

  by.level.data <- model.frame(formula(form,lhs=0,rhs=2), data=data, na.action=NULL)
  by.level <- subset(by.level.data, subset)
  int <- as.character(interaction(by.level.data))

  res <- sapply(split(frame,interaction(by.level)), fun)


  eval.parent(substitute(data[,repvar] <- res[match(int,names(res))]))
}

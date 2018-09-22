#' generates a new variable that is a transformation of existing variables in the dataset or replaces one
#'
#' @param form this argument gives the name of the variable to generate, and, optionally, a set of variables to generate by in the following format: newvar~byvar1+byvar2+....
#' @param value the transformation of the dataset to replace the "newvar" in option \code{form} with.  For example, form="femalewage~year" and value="sum(wage*female)" to get a variable which has yearly wages if female and a zero if the observation is not female.  In Stata, the same command would be: "by year: egen femalewage = total(wage*female)".
#' @param subset only generate values if the condition provided in subset is true.  Make sure to enclose the expression in quotes, like so: subset="female==1 & highschool==1" to generate the values only for women who graduated from highschool.  This option is used like the "if" in Stata.
#' @param replace either TRUE or FALSE.  If FALSE (default), the code refuses to alter the variable if the variable already exists.  Otherwise, if replace=TRUE, then the values will be replaced.
#' @importFrom stats "formula"
#' @importFrom stats "model.frame"
#' @importFrom stats "terms"
#' @export
gen <- function (form, value, subset=NULL, replace=FALSE)
{
  form <- as.Formula(form)
  repvar <- as.character(formula(form,lhs=1,rhs=0))[2]
  if (replace==FALSE && repvar %in% eval(substitute({names(data)}, env=data.env)))
  {
    stop(paste("replace=FALSE and variable ", repvar, " already in data. Call with option replace=TRUE to replace data.",sep=""))
  }
  
  subsetexpr <- subset
  rm(subset)
  eval(substitute({
    if (is.null(subsetexpr))
    {
      subs <- rep(TRUE, times=nrow(data))
    } else {
      subs <- with(data, eval(parse(text=subsetexpr)))
    }

    
    val <- tryCatch({
      formula(form,lhs=0,rhs=1)
      TRUE
    }, warning=function(w) FALSE)
    if (val)
    {
      by.level.data <- model.frame(formula(form,lhs=0,rhs=1), data=data, na.action=NULL)
      by.level <- subset(by.level.data, subs)
    } else {
      by.level.data <- data.frame(by=1:nrow(data))
      by.level <- subset(by.level.data,subs)
    }
    int <- as.character(interaction(by.level.data))
    s <- split(subset(data,subs),interaction(by.level))
    res <- sapply(s,
                  function (u)
                  {
                    return(with(u,eval(parse(text=value))))
                  })
    data[,repvar] <- res[match(int,names(s))]
  }), envir=data.env)

  ls.res <- ls(all=TRUE, envir=data.env)
  rm(list=ls.res[ls.res!="data"],envir=data.env)
}

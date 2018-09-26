#' summarize a variable list, giving basic descriptive statistics
#'
#' @param varlist a variable list either in "var1 var2 x*" form or ~var1+var2+x1+x2+x3 form.
#' @param detail if TRUE, provide a more detailed output for each variable
#' @export
summarize <- function (varlist, detail=FALSE)
{
  if (!inherits(varlist, "formula"))
  {
    varlist <- varlist(varlist)
  }
  vars <- attr(terms(varlist), "term.labels")
  detail <- detail

  for (i in 1:length(vars))
  {
    eval(substitute({
      out <- paste0("Mean: ", mean(data[,vars[i]]),
                    "\nMedian: ", median(data[,vars[i]]),
                    "\nStandard Deviation: ", sd(data[,vars[i]]),
                    "\nMin: ", min(data[,vars[i]]),
                    "\nMax: ", max(data[,vars[i]]))
      if (detail)
      {
        out <- paste0(out,
                      "\n10% Quantile: ", quantile(data[,vars[i]],prob=0.10),
                      "\n25% Quantile: ", quantile(data[,vars[i]],prob=0.25),
                      "\n75% Quantile: ", quantile(data[,vars[i]],prob=0.75),
                      "\n90% Quantile: ", quantile(data[,vars[i]],prob=0.90))
      }
      out <- paste0(out,"\n")
      cat(out)
    }), envir=data.env)
  }
}



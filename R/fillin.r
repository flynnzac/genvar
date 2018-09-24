#' Fully rectangularize a dataset
#'
#' Make the dataset have one observation for every possible interaction of a list of variables.
#' @param varlist a variable list in "var1 var2 var3 x*" format where "*" matches zero or more of any character and "?" matches one of any character (or a varlist in formula format, ~var1+var2+var3+x1+x2+...).  On exit, the data set will contain one observation for every possible interaction of variables with missing values filled in where appropriate.
#' @export
fillin <- function (varlist)
{
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }

  eval(substitute({
    vars <- attr(terms(varlist),"term.labels")
    grid.list <- lapply(vars, function (v) 1:length(unique(data[,v])))
    grid <- expand.grid(grid.list)
    filldata <- data.frame(unique(data[,vars[1]])[grid[,1]])
    if (length(vars) > 1)
    {
      for (i in 2:length(vars))
      {
        filldata <- cbind(filldata, unique(data[,vars[i]])[grid[,i]])
      }
    }

    names(filldata) <- vars
    data <- merge(data,filldata,by=vars,all=TRUE)
    postuse()
    rm(filldata)
    rm(grid)
    rm(grid.list)
    rm(vars)
  }), envir=data.env)
}
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


#' Fully rectangularize a dataset
#'
#' Make the dataset have one observation for every possible interaction of a list of variables.
#' @param varlist a variable list in "var1 var2 var3 x*" format where "*" matches zero or more of any character and "?" matches one of any character (or a varlist in formula format, ~var1+var2+var3+x1+x2+...).  On exit, the data set will contain one observation for every possible interaction of variables with missing values filled in where appropriate.
#' @return returns NULL, invisibly
#' @export
fillin <- function (varlist)
{
  assert_loaded()
  if (!inherits(varlist,"formula"))
  {
    varlist <- varlist(varlist)
  }
  stringify()
  eval(substitute({
    ispanel <- "timevar" %in% names(attributes(data))
    if (ispanel)
    {
      timevar <- attr(data,"timevar")
      obsvar <- attr(data,"obsvar")
    }
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
    if (ispanel)
    {
      xtset(timevar,obsvar)
    }
    rm(filldata)
    rm(grid)
    rm(grid.list)
    rm(vars)

  }), envir=data.env)

  invisible(NULL)
}

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
#' @param varlist a variable list in "var1 var2 var3 x*" format where "*" matches zero or more of any character and "?" matches one of any character (an unquoted list will work as well with one variable).  On exit, the data set will contain one observation for every possible interaction of variables with missing values filled in where appropriate.
#' @return returns NULL, invisibly
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' keepvar("state year emp unemp")
#' addobs("state='Mars',year=1990,emp=100,unemp=4.0")
#' fillin("state year")
#' listif()
#' @export
fillin <- function (varlist)
{
  assert_loaded()
  varlist <- gvcharexpr(enquo(varlist))
  varlist <- structure_varlist(varlist, type="vector")

  stringify()
  eval(substitute({
    ispanel <- "timevar" %in% names(attributes(data))
    if (ispanel)
    {
      timevar <- attr(data,"timevar")
      obsvar <- attr(data,"obsvar")
    }
    grid.list <- lapply(varlist, function (v) 1:length(unique(data[,v])))
    grid <- expand.grid(grid.list)
    filldata <- data.frame(unique(data[,varlist[1]])[grid[,1]])
    if (length(varlist) > 1)
    {
      for (i in 2:length(varlist))
      {
        filldata <- cbind(filldata, unique(data[,varlist[i]])[grid[,i]])
      }
    }

    names(filldata) <- varlist
    data <- merge(data,filldata,by=varlist,all=TRUE)
    postuse()
    if (ispanel)
    {
      xtset(timevar,obsvar)
    }
    rm(filldata)
    rm(grid)
    rm(grid.list)

  }), envir=data.env)

  invisible(NULL)
}

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


#' summarize a variable list, giving basic descriptive statistics
#'
#' @param varlist a variable list either in "var1 var2 x*" form or, optionally, unquoted.
#' @param detail if TRUE, provide a more detailed output for each variable
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' summarize(speed)
#' summarize("speed dist")
#' @export
summarize <- function (varlist, detail=FALSE)
{
  assert_loaded()
  varlist <- gvcharexpr(enquo(varlist))
  detail <- detail

  vars <- structure_varlist(varlist, type="vector")

  for (i in 1:length(vars))
  {
    cat(paste0("---", vars[i], "---", "\n"))
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
  invisible(NULL)
}



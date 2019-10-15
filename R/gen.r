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


#' generates a new variable that is a transformation of existing variables in the dataset or replaces one
#'
#' @param var the name of the variable to be generated
#' @param value the transformation of the dataset to replace the "newvar" in option \code{form} with.  For example,  value="sum(wage*female)" to get a variable which has total female wages.  In Stata, the same command would be: "egen femalewage = total(wage*female)".
#' @param byvar apply the value for each level of the by variables, specified either as a formula, like ~byvar1+byvar2+... or as a varlist "byvar1 byvar2 byvar3...".
#' @param subset only generate values if the condition provided in subset is true.  Make sure to enclose the expression in quotes, like so: subset="female==1 & highschool==1" to generate the values only for women who graduated from highschool.  This option is used like the "if" in Stata.
#' @param replace either TRUE or FALSE.  If FALSE (default), the code refuses to alter the variable if the variable already exists.  Otherwise, if replace=TRUE, then the values will be replaced.
#' @return returns NULL, invisibly
#' @examples
#' use(cars, clear=TRUE)
#' gen(speeddist, speed*dist)
#' listif()
#' @importFrom stats "formula"
#' @importFrom stats "model.frame"
#' @importFrom stats "terms"
#' @importFrom stats "as.formula"
#' @importFrom stats "na.omit"
#' @importFrom rlang "ensym"
#' @importFrom rlang "enquo"
#' @export
gen <- function (var, value, byvar=NULL, subset=NULL, replace=FALSE)
{
  assert_loaded()


  var <- gvcharexpr(enquo(var))
  value <- gvcharexpr(enquo(value))
  
  if (replace==FALSE && var %in% describe())
  {
    stop(paste("replace=FALSE and variable ", var, " already in data. Call with option replace=TRUE to replace data.",sep=""))
  }

  ## evaluate function arguments to ensure done in correct environment
  var <- var
  value <- value
  byvar <- byvar

  subsetexpr <- subset
  rm(subset)
  eval(substitute({
    if (is.null(subsetexpr))
    {
      subs <- rep(TRUE, times=nrow(data))
    } else {
      subs <- with(data, eval(parse(text=subsetexpr)))
    }

    if (!is.null(byvar))
    {
      if (inherits(byvar, "formula"))
      {
        byvar.use <- byvar
      } else {
        byvar.use <- structure_varlist(byvar, type="formula")
      }
      by.level.data <- model.frame(byvar.use, data=data, na.action=NULL)
      by.level <- subset(by.level.data, subs)
      int <- as.character(interaction(by.level.data))
      s <- split(subset(data,subs),interaction(by.level))
      res <- sapply(s,
                    function (u)
                    {
                      return(with(u,eval(parse(text=value))))
                    })
      data[,var] <- res[match(int,names(s))]
    } else {
      data[,var] <- with(data, eval(parse(text=value)))
    }


  }), envir=data.env)

  invisible(NULL)
}

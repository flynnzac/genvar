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


#' reshapes a data set from wide to long or from long to wide formats
#'
#' @param direction either "long" or "wide" to indicate the direction to reorient the data set
#' @param form if direction="long", then the argument should have the form:
#'
#' id1+id2+..~newvar|stub
#'
#' where there are variables in the data set named "stubXXXX" and "newvar" is the name of the new variable that will be added to the data set which will contain the various values of "stubXXXX" on exit.  The variable "stub" on exit will contain the value of "XXXX".  Variables (id1,id2,...) will also be included in the dataset on exit.  The command behaves like "reshape long stub, i(id1 id2 ...) j(newvar)" in Stata.
#'
#' If direction="wide", then the argument should have the form,
#'
#' id1+id2+...~values1+values2+...|byvar1+byvar2+...
#'
#' The variables (id1,id2,...,byvar1,byvar2,...) should uniqely identify observations in the data.  On exit the dataset will contain (id1,id2,...) in addition to values1byvar1.byvar2, values2byvar1.byvar2, ... for each unique value of (byvar1,byvar2,...).  The command behaves like "reshape wide values1 values2 ..., i(id1 id2 ...) j(byvar1...)
#' @examples
#' library(plm)
#' data(Produc)
#' use(Produc, clear=TRUE)
#' listif()
#' shape(state~emp|year, direction="wide")
#' listif()
#' shape(state~year|emp, direction="long")
#' listif()
#' @return returns NULL, invisibly
#' @export
shape <- function (form, direction="long")
{
  assert_loaded()
  if (!is.element(direction,c("long","wide")))
    stop("Direction should be either 'long' or 'wide'.")

  if (direction=="wide")
  {
    eval(substitute(long_to_wide(data, form)), envir=data.env)
  } else {
    eval(substitute(wide_to_long(data, form)), envir=data.env)
  }
  invisible(NULL)
}



long_to_wide <- function (data, form)
{
  if ("reshape" %in% names(data))
    stop("Special variable 'reshape' already in dataset.  Please rename.")

  form <- as.Formula(form)
  by.data <- model.frame(formula(form, lhs=0,rhs=2),data=data)
  id.data <- model.frame(formula(form,lhs=1,rhs=0),data=data)


  id.data[,"reshape"] <- interaction(id.data)

  ## print(length(unique(interaction(by.data,id.data$reshape))))
  ## if (length(unique(interaction(by.data,id.data$reshape))) != nrow(data))
  ##   stop("id variables do not uniquely identify observations.")

  values.data <- cbind(data.frame(reshape=id.data$reshape),model.frame(formula(form,lhs=0,rhs=1),data=data))
  int <- interaction(by.data)
  s <- split(values.data,int)
  for (n in 1:length(s))
  {
    names(s[[n]])[-1] <- paste(names(s[[n]])[-1], names(s)[n],sep="")
    for (ns in 2:ncol(s[[n]]))
    {
      id.data[match(s[[n]]$reshape,id.data$reshape),names(s[[n]])[ns]] <- s[[n]][,ns]
    }
  }

  eval.parent(use(na.omit(id.data),clear=TRUE))

}


## wide_to_long(data, id~newvar|stub)
wide_to_long <- function (data, form)
{
  form <- as.Formula(form)
  stub <- as.character(formula(form,lhs=0,rhs=2))[2]
  newvar <- as.character(formula(form,lhs=0,rhs=1))[2]

  names.long <- names(data)[which(substr(names(data),1,nchar(stub))==stub)]
  fact.level <- substr(names.long,nchar(stub)+1,max(nchar(names(data))))

  id.data <- model.frame(formula(form,lhs=1,rhs=0),data=data)
  if (nrow(unique(id.data)) != nrow(id.data))
    stop("id variables do not uniquely identify observations.")

  s <- lapply (1:length(names.long), function (i)
  {
    new.data <- id.data
    new.data[,newvar] <- fact.level[i]
    new.data[,stub] <- data[,names.long[i]]
    return(new.data)
  })

  eval.parent(substitute(use(do.call("rbind", s),clear=TRUE)))
}


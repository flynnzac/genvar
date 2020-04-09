#' create and manipulate matrices
#'
#' @param command matrix subcommand
#' @param ... arguments to sub-commands
#' @return varies by subcommand
#' @examples
#' todo
#' @export
mat <- function (command, ...)
{
  command <- gvcharexpr(enquo(command))
  m <- mat_command(command)
  m(...)
}

mat_command <- function (command)
{
  if (command=="list")
    mat_list
  else if (command=="set")
    mat_set
  else if (command=="subset")
    mat_subset
  else if (command=="arith")
    mat_arith
}

mat_list <- function (name, ...)
{
  eval(substitute({print(name)}), envir=data.env)
}

mat_set <- function (name, value, Rvalue, ...)
{
  eval(substitute({ name <- value }), envir=data.env)
}


mat_subset <- function (name, matrix1, row_index, col_index, ...)
{
  if (is.logical(row_index))
    row_index <- which(row_index)
  if (is.logical(col_index))
    col_index <- which(col_index)
  
  row_index <- as.vector(row_index)
  col_index <- as.vector(col_index)
  eval(substitute({ name <- matrix(matrix1[row_index,col_index],
                                   nrow=length(row_index),
                                   ncol=length(col_index)) }),
       envir=data.env)
}

#' @importFrom expm expm
#' @importFrom expm logm
mat_arith <- function (name, operation, arg1, arg2, ...)
{
  operation <- gvcharexpr(enquo(operation))

  if (operation=="'")
  {
    eval(substitute({ name <- t(arg1)}), envir=data.env)
  } else if (operation=="*") {
    eval(substitute({ name <- arg1 %*% arg2 }), envir=data.env)
  } else if (operation=="+") {
    eval(substitute({ name <- arg1 + arg2 }), envir=data.env)
  } else if (operation=="-") {
    eval(substitute({ name <- arg1 - arg2 }), envir=data.env)
  } else if (operation==".*") {
    eval(substitute({ name <- arg1 * arg2 }), envir=data.env)
  } else if (operation==".^") {
    eval(substitute({ name <- arg1^arg2 }), envir=data.env)
  } else if (operation=="^") {
    eval(substitute({ name <- expm(logm(arg2*arg1)) }), envir=data.env)
  } else {
    cat("Invalid operation.\n")
  }
}






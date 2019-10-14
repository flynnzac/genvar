#' @importFrom rlang "eval_tidy"
gvcharexpr <- function (u)
{
  ischar <- tryCatch(is.character(eval_tidy(u)),
                     error=function (e) FALSE)

  if (!ischar)
  {
    u <- paste(u)[2]
  } else {
    u <- eval_tidy(u)
  }
  u
}


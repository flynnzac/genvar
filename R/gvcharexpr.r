#' @importFrom rlang "eval_tidy"
#' @importFrom rlang "quo_text"
gvcharexpr <- function (u)
{
  ischar <- tryCatch(is.character(eval_tidy(u)),
                     error=function (e) FALSE)

  if (!ischar)
  {
    u <- quo_text(u)
  } else {
    u <- eval_tidy(u)
  }
  u
}


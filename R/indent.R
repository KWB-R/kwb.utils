#' Indentat a String (Add Spaces to the Left)
#'
#' @param x vector of character
#' @param depth indentation level. Default: 0
#' @param tabLength number of spaces per indentation level. Default: 2
#' @export
indent <- function(x, depth = 0L, tabLength = 2L)
{
  if (depth == 0L) {
    return(x)
  }

  paste0(space(depth, tabLength = tabLength), x)
}

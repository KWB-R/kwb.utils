# createAccessor --------------------------------------------------------------

#' Create Function to Safely Access Data Frame Columns or List Elements
#'
#' @param x a data frame or a list
#' @export
#' @examples
#' getcol <- createAccessor(data.frame(a = 1:2, b = 2:3))
#' getcol("a")
#' getcol("b")
#' getcol(c("b", "a"))
#' # getcol("c") # error with info about existing columns
#'
createAccessor <- function(x)
{
  stopifnot(is.data.frame(x) | is.list(x))

  if (is.data.frame(x)) {
    return(function(columns, ...) selectColumns(x, columns, ...))
  }

  if (is.list(x)) {
    return(function(elements, ...) selectElements(x, elements, ...))
  }
}

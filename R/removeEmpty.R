#' Remove Empty Elements from Vector
#' 
#' @param x vector
#' @return \code{x} with elements for which \code{\link{isNaOrEmpty}} is
#'   \code{TRUE} removed
#' @export
#' @examples 
#' removeEmpty(c(1, NA, 3))
#' removeEmpty(c("a", "", "b", NA, "c", " ", "d"))
removeEmpty <- function(x)
{
  x[! kwb.utils::isNaOrEmpty(x)]
}

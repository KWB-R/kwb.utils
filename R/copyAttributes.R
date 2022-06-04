#' Copy Attributes Between Two Objects
#' 
#' @param x object to which to copy attributes of y
#' @param y object from which to copy attributes to x
#' @param attrNames vector of character containing the names of attributes in 
#'   \code{y} to be copied to \code{x}
#' @export
#' @examples
#' x <- structure(1, a = 2, b = 3)
#' y <- structure(2, c = 4)
#' copyAttributes(x, y, "c")
copyAttributes <- function(x, y, attrNames)
{
  do.call(what = structure, args = c(list(x), lapply(
    stats::setNames(nm = attrNames), getAttribute, x = y
  )))
}

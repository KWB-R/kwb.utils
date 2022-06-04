#' Common Names in two Objects with Names Attribute
#'
#' @param x object with names attribute, e.g. data.frame, named vector
#' @param y object with names attribute, e.g. data.frame, named vector
#' @return vector of names occurring in both \code{x} and \code{y}
#' @export
#' @examples
#' x <- data.frame(a = 1:2, b = 2:3)
#' y <- c(a = 1, c = 2)
commonNames <- function(x, y)
{
  intersect(names(x), names(y))
}

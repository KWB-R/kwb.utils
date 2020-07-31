# textToObject -----------------------------------------------------------------

#' Convert Text Representation Back to R Object
#'
#' @param x vector of character as returned by \code{\link{objectToText}}
#' @return R object
#' @export
#' @examples
#' textToObject("c(1:10)")
#' stopifnot(identical(iris, textToObject(objectToText(iris))))
textToObject <- function(x)
{
  stopifnot(is.character(x))

  dget(textConnection(x))
}

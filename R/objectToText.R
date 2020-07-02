# objectToText -----------------------------------------------------------------

#' Convert R Object to Text Representation
#'
#' @param x R object
#' @return vector of character representing the R code that reproduces the
#'   object \code{x}
#' @importFrom utils capture.output
#' @export
#' @examples
#' objectToText(1:10)
#' objectToText((1:10)[-5])
#' cat(objectToText(head(iris)))
objectToText <- function(x)
{
  utils::capture.output(dput(x))
}

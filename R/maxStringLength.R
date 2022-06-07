#' Maximum String Length in Vector of Character
#'
#' @param x vector of character
#' @return integer representing the length of the longest string in \code{x}
#' @export
#' @examples
#' maxStringLength(c("a", "ab", "abc", "x", "xy-z"))
maxStringLength <- function(x)
{
  x <- as.character(x)
  max(sapply(x, nchar))
}

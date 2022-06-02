#' Split Text at End of Line Characters Into Lines
#'
#' @param x vector of character of length 1.
#' @return vector of character as long as there are lines in \code{x}
#' @export
#' @examples
#' splitIntoLines("a\nb\nc")
#' splitIntoLines("a\r\nb\r\nc")
splitIntoLines <- function(x)
{
  stopifnot(is.character(x), length(x) == 1L)
  strsplit(x, "\r?\n")[[1]]
}

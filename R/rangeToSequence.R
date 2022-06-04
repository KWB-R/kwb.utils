#' Create Sequence from Range
#' 
#' @param x numeric vector with exactly two elements
#' @return integer sequence between \code{x[1]} and \code{x[2]}
#' @export
#' @examples
#' rangeToSequence(c(1, 10))
rangeToSequence <- function(x)
{
  stopifnot(is.numeric(x), length(x) == 2L)
  
  do.call(seq.int, as.list(x))
}

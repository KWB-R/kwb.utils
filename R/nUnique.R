#' Number of Unique Values
#' 
#' @param x R object that function \code{\link{unique}} can be applied on
#' @return Number of unique values in \code{x} (integer)
#' @export
#' @examples
#' nUnique(1:3)
#' nUnique(c(1, 1, 2))
nUnique <- function(x)
{
  length(unique(x))
}

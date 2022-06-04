#' Number of NA values
#' 
#' @param x vector
#' @return number of NA values in \code{x}
#' @export
#' @examples
#' nNA(1:3)
#' nNA(c(1, NA, 3))
nNA <- function(x)
{
  sum(is.na(x))
}

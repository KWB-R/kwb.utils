#' Convert Array to Data Frame
#' 
#' @param x an array
#' @param name name to be given to the value column
#' @return data frame with as many rows as there are elements in the input array
#'   \code{x}. The data frame has one column per dimension of \code{x}
#'   containing the dimension names and one additional column called according
#'   to \code{name} containing the array values.
#' @export
#' @examples
#' x <- array(1:24, dim = c(2, 3, 4), dimnames = list(
#'   c("a", "b"), 
#'   c("x", "y", "z"), 
#'   c("r", "s", "t", "u")
#' ))
#' 
#' arrayToDataFrame(x)
#' 
arrayToDataFrame <- function(x, name = deparse(substitute(x)))
{
  stopifnot(is.array(x))
  
  dim_names <- dimnames(x)
  
  if (is.null(dim_names) || any(sapply(dim_names, is.null))) {
    stop("All dimensions of the array given in 'x' must be named.")
  }
  
  do.call(cbind, c(
    list(do.call(expand.grid, c(dimnames(x), stringsAsFactors = FALSE))), 
    stats::setNames(list(c(x)), name)
  ))
}

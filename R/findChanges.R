#' Find Value Changes in a Vector
#' 
#' Finds the positions in a vector where the value changes an returns the "index
#' regions", i.e. the regions between indices \code{starts_at} and
#' \code{ends_at} whithin each of which the value does not change, i.e. 
#' \code{length(unique(x[starts_at:ends_at])) == 1L} is \code{TRUE}.
#' 
#' The input vector \code{x} must not contain any \code{NA} because it is not
#' clear how to handle this. The function stops with an error if there are any
#' \code{NA} in \code{x}.
#' 
#' @param x vector of atomic mode (e.g. logical, numeric, character)
#' @return data frame with one row more than there are value changes in
#'   \code{x}. If the value in \code{x} does not change from one index to the
#'   next, it is assumed to belong to the same index region. If the value
#'   changes, a new index region begins. In the result data frame each index
#'   region is given by \code{starts_at} and \code{ends_at} which are the
#'   indices of the first and last element, respectively, of each index region.
#'   The function returns \code{NULL} for input vectors \code{x} of length 0.
#' @export
#' @examples
#' findChanges(c(1,2,2,3,3,3))
#' findChanges(c("a", "a", "b", "c", "c", "d"))
#' findChanges(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
#' 
findChanges <- function(x)
{
  if (anyNA(x)) {
    stop("The input vector x must not contain any NA!")
  }
  
  n <- length(x)

  if (n == 0L) {
    return(NULL)
  }

  # Compare values with their direct successors by cutting off the last/first
  # value of the vector. Find the indices where changes occur.
  changes_at <- which(x[-n] != x[-1L]) + 1L 
  
  noFactorDataFrame(
    starts_at = c(1L, changes_at), 
    ends_at = c(changes_at - 1L, n),
    value = x[c(1L, changes_at)]
  )
}

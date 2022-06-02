#' Convert a Named Vector to a Data Frame
#'
#' @param x named vector
#' @return data frame with columns \code{name}, containing the names of \code{x}
#'   and \code{value}, containing the values of \code{x}
#' @export
#' @examples
#' namedVectorToDataFrame(c(a = 1, b = 2, c = 3))
namedVectorToDataFrame <- function(x)
{
  noFactorDataFrame(name = names(x), value = as.character(x))
}

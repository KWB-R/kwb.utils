#' Is Left Substring of X Equal To Y?
#'
#' TODO: Simply use \code{\link{startsWith}}?
#' 
#' @param x String of which the left part is compared with \code{y}
#' @param y String to be compared with the left part of \code{x}
#' @export
#' @examples
#' leftSubstringEquals("Great job", "Great")
#' leftSubstringEquals("Great car", "Great")
#' leftSubstringEquals("Boring job", "Great")
leftSubstringEquals <- function(x, y)
{
  stopifnot(is.character(x), is.character(y))
  
  substr(x, 1, nchar(y)) == y
}

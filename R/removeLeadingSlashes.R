#' Remove Leading Slashes of Strings
#'
#' @param x vector of character
#' @return modified version of \code{x} with all leading slashes in all elements
#'   of \code{x} removed
#' @export
#' @examples
#' removeLeadingSlashes(c("a", "/b", "//c"))
removeLeadingSlashes <- function(x)
{
  gsub("^/+", "", x)
}

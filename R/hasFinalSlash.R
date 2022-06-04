#' Does a String End with Slash?
#' 
#' @param x vector of character
#' @return vector of boolean being \code{TRUE} at indices where the elements in
#'   \code{x} end with a slash ("/")
#' @export
#' @examples
#' (is_directory <- hasFinalSlash(c("a", "b/", "c", "d/")))
hasFinalSlash <- function(x)
{
  n <- nchar(x)
  substr(x, n, n) == "/"
}

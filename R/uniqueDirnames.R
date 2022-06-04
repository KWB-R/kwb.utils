#' Unique Directory Names
#'
#' @param x vector of file paths
#' @return vector of character with the unique directories found in \code{x}
#' @export
#' @examples
#' uniqueDirnames(c("a/b", "a/c", "b/c", "b/d"))
uniqueDirnames <- function(x)
{
  setdiff(unique(dirname(x)), ".")
}

# assertFinalSlash -------------------------------------------------------------

#' Make Sure that Strings End With Slash
#' 
#' @param x vector of character
#' @param method integer value specifying the implementation method. \code{1}
#'   (default): Find strings without ending slash and append slash to these
#'   strings. \code{2}: Remove one or more slashes at the end and append slash
#'   to all strings. \code{3}: Append slash to all strings and replace multiple
#'   occurrences of slash at the end with one slash. Method 1 is the fastest but
#'   does not replace multiple trailing slashes with only one trailing slash 
#'   (see examples).
#' @export
#' @examples 
#' assertFinalSlash(c("a", "b", "c"))
#' assertFinalSlash(c("a/", "b/", "c/"))
#' assertFinalSlash(c("a//", "b", "c/"))
#' 
#' # Use method 2 or 3 to replace multiple slashes with one slash
#' assertFinalSlash(c("a//", "b", "c/"), method = 2)
#' assertFinalSlash(c("a//", "b", "c/"), method = 3)
assertFinalSlash <- function(x, method = 1L)
{
  stopifnot(method %in% 1:3)

  if (method == 1L) {
    # 1. Find strings without ending slash
    # 2. Append slash at the end to these strings
    endpos <- nchar(x)
    no_slash <- substr(x, endpos, endpos) != "/"
    `[<-`(x, no_slash, paste0(x[no_slash], "/"))

  } else if (method == 2L) {

    # 1. Remove one or more slashes at the end
    # 2. Append slash at the end
    paste0(gsub("/+$", "", x), "/")

  } else if (method == 3L) {

    # 1. Append slash at the end
    # 2. Replace multiple occurrences of slash at the end with one slash
    gsub("/+$", "/", paste0(x, "/"))
  }
}

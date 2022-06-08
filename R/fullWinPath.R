#' Windows-Compatible Expanded File Path
#' 
#' Expanded file path with backslashes (each duplicated if 
#' \code{doubleBackslashes} is \code{TRUE}). Duplicated backslashes can be 
#' useful when being used in RMarkdown documents
#' 
#' @param x file path (with slashes or backslashes as separators)
#' @param doubleBackslashes if \code{TRUE} the backslashes in the Windows 
#'   compatible path are duplicated each
#' @return Windows compatible file path with backslashes (duplicated if
#'   \code{doubeBackslashes} is \code{TRUE})
#' @export
#' @examples
#' cat(fullWinPath("~/a/b"))
#' cat(fullWinPath("~/a/b", doubleBackslashes = TRUE))
fullWinPath <- function(x, doubleBackslashes = FALSE)
{
  path <- windowsPath(path.expand(x))
  
  if (! doubleBackslashes) {
    return(path)
  }
  
  gsub("\\\\", "\\\\\\\\", path)
}

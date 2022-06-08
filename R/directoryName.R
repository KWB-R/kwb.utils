#' Safe Version of Base Function dirname()
#' 
#' The base function \code{\link{dirname}} may fail if the path passed is too
#' long. This version checks if the call of \code{dirname()} failed and gives 
#' a clear error message.
#' 
#' @param x a file path of which to get the path to the directory only
#' @return path to directory of file path given in \code{x}
#' @export
#' @examples 
#' \dontrun{
#' directoryName(repeated("verylongpath/", 50))
#' }
directoryName <- function(x)
{
  if (!isTryError(result <- try(base::dirname(x), silent = TRUE))) {
    return(result)
  }
  
  stopFormatted(
    paste(
      "dirname() returned an error for the following path", 
      "(%d characters long):\n",
      "Path: '%s'\n",
      "Error message: '%s'\n"
    ),
    nchar(x), x, as.character(result)
  )
}

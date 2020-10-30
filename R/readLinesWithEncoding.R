# readLinesWithEncoding --------------------------------------------------------

#' Read Lines by Giving the File Encoding
#'
#' @param file a connection object or character string
#' @param \dots arguments passed to \code{\link{readLines}}
#' @param fileEncoding The name of the encoding to be assumed. Passed as
#'   \code{encoding} to \code{\link{file}}, see there.
#' @param encoding passed to \code{\link{readLines}}.
#' @export
#'
readLinesWithEncoding <- function(
  file, ..., fileEncoding = "", encoding = "unknown"
)
{
  # This part is copied from the implementation of read.table
  if (is.character(file)) {
    con <- if (nzchar(fileEncoding)) {
      file(file, "rt", encoding = fileEncoding)
    } else {
      file(file, "rt")
    }
    on.exit(close(con))
  } else {
    con <- file
  }
  
  readLines(con, encoding = encoding, ...)
}

#' Replace File Name Extension
#'
#' Replace the file name extension in a file name or file path
#'
#' @param file file name or full path to a file
#' @param extension new file name extension (including the dot, e.g. ".txt")
#' @export
#' @examples
#' replaceFileExtension(file = "run.bat", ".txt")
replaceFileExtension <- function(file, extension)
{
  paste0(removeExtension(file), extension)
}

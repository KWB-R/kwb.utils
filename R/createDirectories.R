#' Create Directories
#'
#' @param paths paths to directories to be created
#' @param \dots further arguments passed to \code{\link{createDirectory}}
#' @return paths to created directories
#' @export
createDirectories <- function(paths, ...)
{
  unlist(lapply(unique(paths), createDirectory, ...))
}

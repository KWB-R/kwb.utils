#' Create Function to Compose Path in Package
#'
#' @param package name of package for which to create a shortcut to
#'   \code{system.file("extdata", ..., package = package)}
#' @return This function returns a function that internally calls
#'   \code{system.file("extdata", ..., package = package)}
#' @export
#' @examples
#' extdataFile <- createFunctionExtdataFile("kwb.utils")
#' dir(extdataFile())
createFunctionExtdataFile <- function(package)
{
  function(..., must_exist = FALSE) {
    
    extdata_dir <- system.file("extdata", package = package)

    if (must_exist) {
      safePath(extdata_dir, ...)
    } else {
      do.call(file.path, list(extdata_dir, ...))
    }
  }
}

#' Path to File in Installed Package
#' 
#' @param \dots parts of the file path to be passed to \code{\link{system.file}}
#' @param must_exist if \code{TRUE} and the specified file does not exist, the 
#'   program stops with an error message
#' @return path to file in the package installation folder in the R library
#'   or "" if the path does not exist
#' @export
#' @examples 
#' # List the files provided in the "extdata" folder of kwb.utils
#' dir(extdataFile())
extdataFile <- createFunctionExtdataFile("kwb.utils")

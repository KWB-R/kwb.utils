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
  function(..., must_exist = TRUE, dbg = FALSE) {

    lib.loc = .libPaths()
    
    packagePath <- find.package(package, lib.loc, quiet = FALSE, verbose = dbg)
    
    printIf(dbg, packagePath)
    
    extdata_dir <- system.file(
      "extdata", 
      package = package, 
      lib.loc = lib.loc,
      mustWork = TRUE
    )
    
    if (!nzchar(extdata_dir)) {
      stopFormatted(
        "Could not determine path to 'extdata' folder of package '%s'",
        package
      )
    }
    
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
#' @param must_exist if \code{TRUE} (the default) and the specified file does 
#'   not exist, the program stops with an error message
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) debug messages are 
#'   shown
#' 
#' @return path to file in the package installation folder in the R library
#'   or "" if the path does not exist
#' @return path to the specified file
#' @export
#' @examples 
#' # List the files provided in the "extdata" folder of kwb.utils
#' dir(extdataFile())
extdataFile <- createFunctionExtdataFile("kwb.utils")

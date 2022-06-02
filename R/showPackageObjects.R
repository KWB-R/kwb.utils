#' Show Names of Objects/Functions in a Package
#'
#' @param package name of package of which to show the functions
#' @param show if \code{TRUE} (the default) the function names are printed to
#'   the console otherwise the vector of function names is returned
#' @export
showPackageObjects <- function(package = "kwb.utils", show = TRUE)
{
  objectNames <- ls(getNamespace(package))

  if (show) {
    writeLines(sprintf("%3d: %s", seq_along(objectNames), objectNames))
    return(invisible(objectNames))
  }

  objectNames
}

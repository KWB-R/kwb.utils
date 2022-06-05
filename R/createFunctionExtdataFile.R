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
  function(...) system.file("extdata", ..., package = package)
}

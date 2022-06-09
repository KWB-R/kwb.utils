
#' Create Function to Provide Package Objects in Global Environment
#'
#' @param package name of package for which to create a shortcut to
#'   \code{assignPackageObjects(package = package)}
#' @return This function returns a function that internally calls
#'   \code{assignPackageObjects(package = package)}
#' @export
#' @examples
#' provideFunctions <- createFunctionAssignObjects("kwb.utils")
#' \dontrun{
#' provideFunctions()
#' }
createFunctionAssignObjects <- function(package)
{
  function() assignPackageObjects(package)
}

#' Assign Package Functions to the Global Environment
#' 
#' This function provides all (also non-exported) function definitions of this 
#' package in the Global environment. This is useful for debugging the code
#' of a function that calls non-exported functions.
#' 
#' @export
#' @examples 
#' \dontrun{
#' # Provide all functions of kwb.utils in the global environment
#' assignObjects()
#' }
assignObjects <- createFunctionAssignObjects("kwb.utils")

# assignArgumentDefaults -------------------------------------------------------

#' Assign Argument Default Values in the Global Environment
#' 
#' @param FUN a function (a closure or a primitive). If \code{FUN} is a
#'   character string then the function with that name is found and used.
#'   
#' @examples 
#' # Assign the default values of breakInSequence() in the global environment
#' assignArgumentDefaults(kwb.utils::breakInSequence)
#' 
#' # The argument "expectedDiff" is now in the global environment
#' ls()
#' 
#' # Its value is 1 which is the default value defined in the function definition
#' expectedDiff
#' 
assignArgumentDefaults <- function(FUN)
{
  # Get formal argument list
  arguments <- as.list(args(FUN))
  
  # Remove last entry representing the body of the function
  arguments <- arguments[- length(arguments)]
  
  # Remove arguments without defaults
  arguments <- arguments[! sapply(arguments, is.symbol)]
  
  # Evaluate the expressions
  arguments <- lapply(arguments, eval, envir = arguments)
  
  # Assign all arguments with defaults in the global environment
  assignAll(arguments, envir = .GlobalEnv)
}

# .optionName_loaded -----------------------------------------------------------

#'  optionName loaded
#' 
#' 
.optionName_loaded <- "kwb.utils.loaded"

# isLoaded ---------------------------------------------------------------------

#' Has a script been loaded (with source)?
#' 
#' Check whether a script has been loaded (with source) or not. The information
#'   is read from a list that stores a logical value (\code{TRUE} or \code{FALSE}) for 
#'   each script name for which \code{\link{setLoaded}} has been called.
#' 
#' @param scriptName name of the script for which to get the "loaded" state
#' 
#' @seealso \code{\link{setLoaded}} (see example there)
#' 
#' @examples 
#'   # For an example see kwb.utils::setLoaded()...
#'   
#'   # In fact, the information about loaded scripts is stored in the
#'   # R option "kwb.utils.loaded":
#'   setLoaded("myScript")
#'   
#'   options("kwb.utils.loaded")[[1]]
#'   
#' 
isLoaded <- structure(function # Has a script been loaded (with source)?
### Check whether a script has been loaded (with source) or not. The information
### is read from a list that stores a logical value (\code{TRUE} or \code{FALSE}) for 
### each script name for which \code{\link{setLoaded}} has been called.
(
  scriptName
  ### name of the script for which to get the "loaded" state
)
{
  ##seealso<< \code{\link{setLoaded}} (see example there)
  
  loaded <- defaultIfNULL(getOption(.optionName_loaded), list())
  
  defaultIfNULL(loaded[[scriptName]], FALSE)
}, ex = function() {
  # For an example see kwb.utils::setLoaded()...
  
  # In fact, the information about loaded scripts is stored in the
  # R option "kwb.utils.loaded":
  setLoaded("myScript")
  
  options("kwb.utils.loaded")[[1]]
})

# setLoaded --------------------------------------------------------------------

#' set the "loaded" status for a script
#' 
#' You may use this function at the last line of your script to indicate that
#'   this script has already been loaded. This information is stored in the R
#'   option "kwb.utils.loaded" from which you may read the information back with
#'   \code{\link{isLoaded}}.
#' 
#' @param scriptName name of the script for which we want to set the "loaded" state
#' @param isLoaded logical. Use \code{TRUE} to indicate that the script \code{scriptName}
#'   has been loaded and use \code{FALSE} to indicate that the script is not
#'   loaded (e.g. because you cleared the workspace in the meanwhile).
#' 
#' @seealso \code{\link{isLoaded}}
#' 
#' @examples 
#'   # If you have a script with the main part on top and the required functions
#'   # defined below (as recommended by Robert C. Martin, the author of "Clean
#'   # Code") your script may look like this:
#'   
#'   # Main part -----
#'   
#'   # Check if the script has already been loaded (i.e. if setLoaded() has been 
#'   # called, see end of script). If yes, we can enter the main section. Otherwise
#'   # we have to skip the main section since the function sayHello() is not yet
#'   # defined.
#'   if (isLoaded("welcome")) {
#'     sayHello(who = "Hauke")
#'   }
#'   
#'   # Functions -----
#'   sayHello <- function(who) {
#'     clearConsole()
#'     cat("***\n***\n*** Hello", who, "how are you?\n***\n***\n")
#'   }
#'   
#'   # At the end of your script, call setLoaded() to indicate that your script is
#'   # loaded now. If you "source" the script a second time, isLoaded("welcome") 
#'   # will return TRUE and thus the main section will be entered...
#'   setLoaded("welcome")
#'   
#' 
setLoaded <- structure(function # set the "loaded" status for a script
### You may use this function at the last line of your script to indicate that
### this script has already been loaded. This information is stored in the R
### option "kwb.utils.loaded" from which you may read the information back with
### \code{\link{isLoaded}}.
(
  scriptName, 
  ### name of the script for which we want to set the "loaded" state
  isLoaded = TRUE
  ### logical. Use \code{TRUE} to indicate that the script \code{scriptName}
  ### has been loaded and use \code{FALSE} to indicate that the script is not
  ### loaded (e.g. because you cleared the workspace in the meanwhile).
)
{
  ##seealso<< \code{\link{isLoaded}}
  stopifnot(is.logical(isLoaded) && length(isLoaded) == 1)
  
  # Get the R option (list of booleans)
  loaded <- defaultIfNULL(getOption(.optionName_loaded), list())
    
  # Update the list of booleans for the given script name
  loaded[[scriptName]] <- isLoaded
  
  # Set the R option
  do.call(options, args = structure(list(loaded), names = .optionName_loaded))
}, ex = function() {
  # If you have a script with the main part on top and the required functions
  # defined below (as recommended by Robert C. Martin, the author of "Clean
  # Code") your script may look like this:
  
  # Main part -----
  
  # Check if the script has already been loaded (i.e. if setLoaded() has been 
  # called, see end of script). If yes, we can enter the main section. Otherwise
  # we have to skip the main section since the function sayHello() is not yet
  # defined.
  if (isLoaded("welcome")) {
    sayHello(who = "Hauke")
  }
  
  # Functions -----
  sayHello <- function(who) {
    clearConsole()
    cat("***\n***\n*** Hello", who, "how are you?\n***\n***\n")
  }
  
  # At the end of your script, call setLoaded() to indicate that your script is
  # loaded now. If you "source" the script a second time, isLoaded("welcome") 
  # will return TRUE and thus the main section will be entered...
  setLoaded("welcome")
})

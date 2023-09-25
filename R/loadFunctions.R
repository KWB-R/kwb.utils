#' Load Functions from Scripts into Attached Namespace
#' 
#' This function parses R scripts at the given paths and provides all objects in
#' an environment that is attached to the search path.
#' 
#' @param paths vector of character with paths to files (R scripts) or 
#'   directories (in which to look for R scripts).
#' @param attach if \code{TRUE} the environment with all parsed objects is
#'   attached to the search path, otherwise the environment is returned
#' @param name name that is used when attaching the environment with all parsed
#'   objects to the search path. Default: kwb
#' @param dbg logical indicating whether or not to show debug messages
#' @return if \code{attach} is \code{FALSE} the environment with all parsed 
#'   objects is returned, otherwise \code{NULL}, invisibly.
#' @export
loadFunctions <- function(paths, attach = TRUE, name = "kwb", dbg = TRUE)
{
  # Which paths refer to directories?
  isDirectory <- file.info(paths)[, "isdir"]
  
  # List all R script files
  files <- dir(paths[isDirectory], full.names = TRUE, "\\.R$")

  # Add the paths that refer to (script) files
  files <- c(files, paths[!isDirectory])
  
  # Create a new environment in which to store all functions
  resultEnv <- new.env()
  
  for (file in files) {
    
    catAndRun(
      paste("Loading functions from", file), 
      dbg = dbg,
      expr = {
        
        # Create a new environment in which to store the functions of the script
        scriptEnv <- new.env()
        
        # Load the functions defined in the file into the script environment
        source(file, local = scriptEnv)
        
        # Check for duplicated names in result and script environment
        duplicates <- intersect(ls(scriptEnv), ls(resultEnv))
        
        # Stop if there are duplicates
        if (length(duplicates)) {
          stopFormatted(
            "The function(s) are already defined: %s", 
            stringList(duplicates)
          )
        }
        
        # Copy all elements from script environment to result environment
        resultEnv <- mergeEnvironments(resultEnv, scriptEnv)
      }
    )
  }

  if (!attach) {
    return(resultEnv)
  }
  
  # Detach the environment if it is already attached
  if (name %in% search()) {
    detach(name, character.only = TRUE)
  }
  
  # Attach the environment
  attach(resultEnv, name = name)
}

# mergeEnvironments ------------------------------------------------------------

# Helper function to copy the elements of one environment to another
mergeEnvironments <- function(targetEnv, sourceEnv)
{
  # Assign all elements of the source environment in the target environment
  for (name in ls(sourceEnv, all.names = TRUE)) {
    assign(name, get(name, sourceEnv), targetEnv)
  }
  
  # Return the target environment
  targetEnv
}

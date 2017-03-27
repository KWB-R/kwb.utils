# sourceScripts ----------------------------------------------------------------

#' load R scripts with \code{source}
#' 
#' load R scripts with \code{source}
#' 
#' @param scripts full paths to R scripts
#' @param dbg if TRUE (default) log messages ("loading... ok") are shown
#' 
#' @return the vector of script paths is returned invisibly
#' 
sourceScripts <- function # load R scripts with \code{source}
### load R scripts with \code{source}
(
  scripts, 
  ### full paths to R scripts
  dbg = TRUE
  ### if TRUE (default) log messages ("loading... ok") are shown
)
{
  for (script in scripts) {
    cat("loading functions from ", basename(script), "... ")
    if (file.exists(script)) {
      source(script)      
    } else {
      warning("Skipping non-existing script: ", script)
    }
    cat("ok.\n")
  }
  
  invisible(scripts)
  ### the vector of script paths is returned invisibly
}

# runInDirectory ---------------------------------------------------------------

#' change working dir and run function 
#' 
#' change working directory and run function 
#' 
#' @param target.dir target directory. Default: tempdir()
#' @param FUN function to be invoked
#' @param \dots arguments to be passed to function FUN
#' @param .dbg if TRUE, debug messages on changing the directory are shown. Default:
#'   \code{FALSE}
#' 
runInDirectory <- function # change working dir and run function 
### change working directory and run function 
(
  target.dir = tempdir(), 
  ### target directory. Default: tempdir()
  FUN,
  ### function to be invoked
  ...,
  ### arguments to be passed to function FUN
  .dbg = FALSE
  ### if TRUE, debug messages on changing the directory are shown. Default:
  ### \code{FALSE}
) 
{
  # Save current working directory, set working directory to tdir
  # and reset working directory on exit
  current.dir <- getwd()
  
  catIf(.dbg, sprintf("Changing current directory to \"%s\"... ", target.dir))
  
  setwd(target.dir)
  
  catIf(.dbg, "ok.\n")
  
  on.exit(setwd(current.dir))
  
  FUN(...)
}

# defaultWindowsProgramFolders -------------------------------------------------

#' default windows program folders
#' 
#' default windows program folders
#' 
defaultWindowsProgramFolders <- function # default windows program folders
### default windows program folders
(
)
{
  directories <- c(
    english = "C:/Program Files", # English / Dutch / Russian / Polish / Czech / Chinese
    german = "C:/Programme", # German  
    spanish = "C:/Archivos de programa", # Spanish
    french = "C:/Programmes", # French
    italian ="C:/Programmi", # Italian
    portuguese = "C:/Arquivos de Programas", # Portuguese
    swedish = "C:/Program", # Swedish
    danish = "C:/Programmer", # Danish
    norwegian = "C:/Programfiler", # Norwegian
    romanian = "C:/Fisiere Program" # Romanian
  )
  
  directories.x86 <- paste(directories, "(x86)")
  names(directories.x86) <- names(directories)
  
  c(directories, directories.x86)
}

# mySystemTime -----------------------------------------------------------------

#' Elapsed Time of a Function Call
#' 
#' Call a function and show the elapsed time using \code{system.time})
#' 
#' @param FUN function to be called
#' @param args list of arguments passed to \code{FUN}
#' 
mySystemTime <- function(FUN, args)
{
  stime <- system.time(returnValue <- do.call(FUN, args))
  cat(sprintf("Elapsed time: %0.2f s\n", stime["elapsed"]))
  returnValue
}

# runBatchfileInDirectory ------------------------------------------------------

#' runBatchfileInDirectory
#' 
#' runBatchfileInDirectory
#' 
#' @param batchfile full path to Windows batch file
#' @param directory directory from which batchfile is to be invoked. Default: directory
#'   of batch file
#' @param \dots arguments passed to shell.exec
#' 
runBatchfileInDirectory <- function # runBatchfileInDirectory
### runBatchfileInDirectory
(
  batchfile, 
  ### full path to Windows batch file
  directory = dirname(batchfile),
  ### directory from which batchfile is to be invoked. Default: directory
  ### of batch file
  ...
  ### arguments passed to shell.exec
)
{
  currentdir <- getwd()
  on.exit(setwd(currentdir))
  
  setwd(directory)
  shell.exec(batchfile, ...)
}

# cmdLinePath ------------------------------------------------------------------

#' Path in Quotes for Usage in Command Line
#' 
#' Set the given path in quotes so that in can be used in a command line
#' #' 
#' @param x path to be quoted
cmdLinePath <- function # cmdLinePath
### cmdLinePath
(
  x
)
{
  hsQuoteChr(windowsPath(x), '"')
}

# .debugMessageIf --------------------------------------------------------------

#'  debugMessageIf
#' 
#' 
.debugMessageIf <- function(dbg, ...)
{
  if (dbg) {
    cat(...)
  }  
}

# copyDirectoryStructure -------------------------------------------------------

#' copy directory structure
#' 
#' copy the full directory structure from a source directory to a target
#'   directory
#' 
#' @param sourcedir path to the source directory
#' @param targetdir path to the target directory
#' @param excludePattern pattern matching directory names to be excluded from the copying process
#' @param recursive if TRUE (default) the full tree of directories and subdirectories is
#'   copied, otherwise only the top-level directories
#' @param dbg if TRUE (default) debug messages are shown
#' 
#' @return This function invisibly returns a vector of character containing the full
#'   paths of the directories that were created.
#' 
copyDirectoryStructure <- function # copy directory structure
### copy the full directory structure from a source directory to a target
### directory
(
  sourcedir, 
  ### path to the source directory
  targetdir, 
  ### path to the target directory
  excludePattern = "^$", 
  ### pattern matching directory names to be excluded from the copying process
  recursive = TRUE,
  ### if TRUE (default) the full tree of directories and subdirectories is
  ### copied, otherwise only the top-level directories
  dbg = TRUE
  ### if TRUE (default) debug messages are shown
)
{
  subdirs <- list.dirs(sourcedir, recursive = recursive, full.names = FALSE)
  
  subdirs <- subdirs[! grepl(excludePattern, basename(subdirs))]
  
  targetpaths <- file.path(targetdir, subdirs)
  
  paths <- as.character(lapply(targetpaths, createDirAndReturnPath, dbg = dbg))
  
  invisible(paths)
  ### This function invisibly returns a vector of character containing the full
  ### paths of the directories that were created.
}

# createDirAndReturnPath -------------------------------------------------------

#' create directory if it does not exist
#' 
#' create directory if it does not exist
#' 
#' @param dir.to.create character string representing the path to the directory
#'   to be created
#' @param dbg if \code{TRUE} messages about created or found directories are
#'   shown
#' @param confirm if \code{TRUE} (the default is \code{FALSE}!) the user is
#'   asked to confirm the creation of a directory
createDirAndReturnPath <- function
(
  dir.to.create,
  dbg = TRUE,
  confirm = FALSE  
) 
{ 
  stopifnot(length(dir.to.create) == 1 && is.character(dir.to.create))
  
  dir.name <- sprintf("The directory \"%s\"", dir.to.create)
  
  # Return the directory name if it already exists
  if (file.exists(dir.to.create)) {
    .debugMessageIf(dbg, dir.name, "already exists.\n")
    return(dir.to.create)
  }
  
  # Does the parent directory exist?
  parentDir <- dirname(dir.to.create)
  
  if (! file.exists(parentDir)) {
    .debugMessageIf(dbg, "The parent directory", parentDir, 
                    "needs to be created first.\n")
    parentDir <- createDirAndReturnPath(parentDir, dbg, confirm)
  }
  
  # Return NULL if the parent directory could not be created
  if (is.null(parentDir)) {
    return (NULL)
  }
  
  # Continue ony if continuation was confirmed
  if (confirm) {
    continue <- (readline(paste("Create folder", hsQuoteChr(dir.to.create), 
                                "(Y,n)? ")) == "Y")
  }
  else {
    continue <- TRUE
  }
  
  # Return NULL if user does not want to continue
  if (!continue) {
    return (NULL)
  }
  
  if (! dir.create(dir.to.create)) {
    stop(dir.name, " could not be created.")
  }
  
  .debugMessageIf(dbg, dir.name, "was created.\n")        
  return (dir.to.create)
}

# tempSubdirectory -------------------------------------------------------------

#' create and return path of subdirectory in temp()
#' 
#' create and return path of subdirectory in temp()
#' 
#' @param subdir name of subdirectory to be created
#' 
#' @return full path to created directory
#' 
tempSubdirectory <- function # create and return path of subdirectory in temp()
### create and return path of subdirectory in temp()
(
  subdir
  ### name of subdirectory to be created
)
{
  sdir <- file.path(tempdir(), subdir)
  if (! file.exists(sdir)){
    dir.create(sdir)    
  }
  
  sdir
  ### full path to created directory
}

# hsOpenWindowsExplorer --------------------------------------------------------

#' Open Windows Explorer
#' 
#' open Windows Explorer
#' 
#' @param startdir directory to be opened in Windows Explorer
#' @param use.shell.exec if \code{TRUE} \code{shell.exec} is used instead of
#'   running the system command \code{cmd /C explorer}
#' 
hsOpenWindowsExplorer <- function
(
  startdir = tempdir(),
  use.shell.exec = ! .isNetworkPath(startdir)
)
{
  if (use.shell.exec) {
    shell.exec(file = startdir)
  }
  else {
    #hsShell(sprintf("explorer /e,/root,\"%s\"", windowsPath(startdir)))    
    system(paste("cmd /C explorer", windowsPath(startdir)))
  }  
}

# .isNetworkPath ---------------------------------------------------------------

#'  isNetworkPath
#' 
#' 
.isNetworkPath <- function(x) 
{
  grepl("^(//|\\\\\\\\)", x)
}

# windowsPath ------------------------------------------------------------------

#' convert to MS Windows-compatible path
#' 
#' create MS Windows-compatible path by substituting forward slashes with 
#'   backslashes
#' 
#' @param path vector of character representing file paths
windowsPath <- function
(
  path
) 
{
  gsub("/", "\\\\", path)  
}

# rStylePath -------------------------------------------------------------------

#' R compatible file path
#' 
#' R compatible file path with backslashes replaced with forward slashes
#' 
#' @param path character string representing a file path
#' 
#' @return path in which backslashes are replaced with forward slashes
#' 
rStylePath <- function(path) 
{
  gsub("\\\\", "/", path)
}

# .showCommand -----------------------------------------------------------------

#'  showCommand
#' 
#' 
.showCommand <- function(commandLine)
{
  cat(sprintf("Running command: >>>%s<<<\n", commandLine))  
}

# hsSystem ---------------------------------------------------------------------

#' Wrapper around "system"
#' 
#' wrapper around "system"
#' 
#' @param commandLine character string passed to \code{system}
#' @param ... additional arguments passed to \code{system}
hsSystem <- function(commandLine, ...)
{
  .showCommand(commandLine)
  system(command=commandLine, ...)  
}

# hsShell ----------------------------------------------------------------------

#' wrapper around "shell"
#' 
#' wrapper around "shell"
#' 
#' @param commandLine character string passed to \code{shell}
#' @param ... additional arguments passed to \code{shell}
#' 
hsShell <- function(commandLine, ...)
{
  .showCommand(commandLine)
  shell(commandLine, ...)  
}

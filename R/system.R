# objectSize -------------------------------------------------------------------

#' Object Size and Sizes of Sub Structures in Mb
#'
#' @param x object
#' @param max_depth number of sub levels of list structures to be shown
#' @param units passed to \code{\link{object.size}}
#' @param depth depth of recursive call (for internal use only)
#'
#' @return if \code{x} is a list, a list of the same structure is returned with
#'   each list elemenet replaced by its size, otherwise the object size of
#'   \code{x}. Each list or sub list with more than one element is assigned an
#'   attribute "total" to containing the total size of the list.
#'
objectSize <- function(x, max_depth = 2, units = "auto", depth = 0)
{
  get_size <- function(xx) format(utils::object.size(xx), units = units)

  result <- if (is.list(x) && depth < max_depth) {

    sizes <- lapply(x, objectSize, max_depth, units, depth + 1)

    if (length(sizes) > 1) {

      sizes <- structure(sizes, total = get_size(x))
    }

  } else {

    get_size(x)
  }

  if (depth == 0) {

    utils::str(result)
  }

  invisible(result)
}

# safePath ---------------------------------------------------------------------

#' Stop if Path does not Exist
#'
#' Check if file or folder path exists and return the path or stop if the path
#' does not exist
#'
#' @param \dots character vectors passed to \code{file.path}
#'
#' @return path as given in \code{path}
#'
safePath <- function(...)
{
  file <- file.path(...)

  if (! file.exists(file)) {

    dirpath <- dirname(file)

    if (! file.exists(dirpath)) {

      stop("No such directory: ", dirpath, call. = FALSE)

    } else {

      stop(call. = FALSE, sprintf(
        "No such file: '%s' in\n  '%s'.\nAvailable files:\n  %s",
        basename(file), dirpath, stringList(dir(dirpath), collapse = "\n  ")
      ))
    }
  }

  file
}

# .OStype ----------------------------------------------------------------------

#' see tools:::.OStype
#'
#' This is a copy of tools:::.OStype. It is redefined here so that it can
#'   be used within this package. R CMD build would complain if I used
#'   tools:::.OStype!
#'
.OStype <- function()
{
  OS <- Sys.getenv("R_OSTYPE")

  if (nzchar(OS)) {
    
    OS
    
  } else {
    
    .Platform$OS.type
  }
}

# desktop ----------------------------------------------------------------------

#' Path to Your Desktop
#'
#' Get the path to your desktop
#'
#' @param osType Optional. Type of operating system, one of \code{"unix"},
#'   \code{"windows"}
#'   
#' @return character string representing the path to your desktop
#'
desktop <- function(osType = .OStype())
{
  desktops <- c(
    windows = "<userprofile>/Desktop",
    unix = "/home/<user>/Desktop"
  )

  if (isNaOrEmpty(desktops[osType])) {

    stop("I do not know where the desktop is in: ", osType)
  }

  dict <- list(desktop = desktops[osType])

  if (osType == "windows") {
    
    hsResolve(
      "desktop", dict,
      userprofile = kwb.utils::rStylePath(Sys.getenv("USERPROFILE"))
    )

  } else {
    
    hsResolve("desktop", dict, user = user())
  }
}

# user -------------------------------------------------------------------------

#' Name of the Current User
#'
#' Get the name of the current user from the environment variables
#'
#' @param osType Optional. Type of operating system, one of \code{"unix"},
#'   \code{"windows"}
#'   
#' @return character string represenging the name of the current user
#'
user <- function(osType = .OStype())
{
  user <- Sys.getenv(c(windows = "USERNAME", unix = "USER")[osType])

  if (isNaOrEmpty(user)) {
    
    stop("I do not know how to get the user name in: ", osType)
  }

  user
}

# sourceScripts ----------------------------------------------------------------

#' Load R Scripts with \code{source}
#'
#' @param scripts full paths to R scripts
#' @param dbg if TRUE (default) log messages ("loading... ok") are shown
#'
#' @return the vector of script paths is returned invisibly
#'
sourceScripts <- function(scripts, dbg = TRUE)
{
  for (script in scripts) {

    catIf(dbg, "loading functions from", basename(script), "... ")

    if (file.exists(script)) {
      
      source(script)
      
    } else {
      
      warning("Skipping non-existing script: ", script)
    }

    catIf(dbg, "ok.\n")
  }

  invisible(scripts)
}

# runInDirectory ---------------------------------------------------------------

#' Change Working Directory and Run Function
#'
#' @param target.dir target directory. Default: \code{tempdir()}
#' @param FUN function to be invoked
#' @param \dots arguments to be passed to function FUN
#' @param .dbg if TRUE, debug messages on changing the directory are shown.
#'   Default: \code{FALSE}
#'
runInDirectory <- function(target.dir = tempdir(), FUN, ..., .dbg = FALSE)
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

#' Default Windows Program Folders
#'
defaultWindowsProgramFolders <- function()
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

#' Run a Batch File in a given Directory
#'
#' @param batchfile full path to Windows batch file
#' @param directory directory from which batchfile is to be invoked. Default:
#'   directory of batch file
#' @param \dots arguments passed to shell.exec
#'
runBatchfileInDirectory <- function(
  batchfile, directory = dirname(batchfile), ...
)
{
  currentdir <- getwd()
  
  on.exit(setwd(currentdir))

  setwd(directory)
  
  if (.OStype() == "windows") {
    
    shell.exec(batchfile, ...)
    
  } else {
    
    stop("Not implemented for unix")
  }
}

# cmdLinePath ------------------------------------------------------------------

#' Path in Quotes for Usage in Command Line
#'
#' Set the given path in quotes so that in can be used in a command line
#' 
#' @param x path to be quoted
#' 
cmdLinePath <- function(x)
{
  hsQuoteChr(windowsPath(x), '"')
}

# copyDirectoryStructure -------------------------------------------------------

#' Copy Directory Structure
#' 
#' Copy the full directory structure from a source directory to a target 
#' directory
#'
#' @param sourcedir path to the source directory
#' @param targetdir path to the target directory
#' @param excludePattern pattern matching directory names to be excluded from
#'   the copying process
#' @param recursive if TRUE (default) the full tree of directories and
#'   subdirectories is copied, otherwise only the top-level directories
#' @param dbg if TRUE (default) debug messages are shown
#'
#' @return This function invisibly returns a vector of character containing the
#'   full paths of the directories that were created.
#'
copyDirectoryStructure <- function(
  sourcedir, targetdir, excludePattern = "^$", recursive = TRUE, dbg = TRUE
)
{
  subdirs <- list.dirs(sourcedir, recursive = recursive, full.names = FALSE)

  subdirs <- subdirs[! grepl(excludePattern, basename(subdirs))]

  targetpaths <- file.path(targetdir, subdirs)

  paths <- as.character(lapply(targetpaths, createDirectory, dbg = dbg))

  invisible(paths)
}

# createDirAndReturnPath -------------------------------------------------------

#' Create a Directory including required "upward" Folders
#'
#' @param path character string representing the path to the directory to be
#'   created
#' @param dbg if \code{TRUE} messages about created or found directories are
#'   shown
#' @param confirm if \code{TRUE} (the default is \code{FALSE}!) the user is
#'   asked to confirm the creation of a directory
#' @return created path or \code{NULL} if the path could not be created
#'
createDirAndReturnPath <- function(path, dbg = TRUE, confirm = FALSE)
{
  warningDeprecated("createDirAndReturnPath", "createDirectory")

  createDirectory(path, dbg = TRUE, confirm = FALSE)
}

# createDirectory --------------------------------------------------------------

#' Create Directory if it does not exist
#'
#' @param dir.to.create character string representing the path to the directory
#'   to be created
#' @param dbg if \code{TRUE} messages about created or found directories are
#'   shown
#' @param confirm if \code{TRUE} (the default is \code{FALSE}!) the user is
#'   asked to confirm the creation of a directory
#' @return created path or \code{NULL} if the path could not be created
#'
createDirectory <- function(dir.to.create, dbg = TRUE, confirm = FALSE)
{
  stopifnot(length(dir.to.create) == 1, is.character(dir.to.create))

  dir.name <- sprintf("The directory \"%s\"", dir.to.create)

  # Return the directory name if it already exists
  if (file.exists(dir.to.create)) {

    catIf(dbg, dir.name, "already exists.\n")

    return(dir.to.create)
  }

  # Does the parent directory exist?
  parentDir <- dirname(dir.to.create)

  if (! file.exists(parentDir)) {

    catIf(
      dbg, "The parent directory", parentDir, "needs to be created first.\n"
    )

    parentDir <- createDirectory(parentDir, dbg, confirm)
  }

  # Return NULL if the parent directory could not be created
  if (is.null(parentDir)) {

    return (NULL)
  }

  # Continue ony if continuation was confirmed
  continue <- if (confirm) {

    prompt <- paste("Create folder", hsQuoteChr(dir.to.create), "(Y,n)? ")

    readline(prompt) == "Y"

  } else {

    TRUE
  }

  # Return NULL if user does not want to continue
  if (! continue) {

    return (NULL)
  }

  if (! dir.create(dir.to.create)) {

    stop(dir.name, " could not be created.")
  }

  catIf(dbg, dir.name, "was created.\n")

  return (dir.to.create)
}

# tempSubdirectory -------------------------------------------------------------

#' Create and Return Path of Subdirectory in temp()
#'
#' @param subdir name of subdirectory to be created
#'
#' @return full path to created directory
#'
tempSubdirectory <- function(subdir)
{
  sdir <- file.path(tempdir(), subdir)

  if (! file.exists(sdir)) {

    dir.create(sdir)
  }

  sdir
}

# hsOpenWindowsExplorer --------------------------------------------------------

#' Open Windows Explorer
#'
#' @param startdir directory to be opened in Windows Explorer
#' @param use.shell.exec if \code{TRUE} \code{shell.exec} is used instead of
#'   running the system command \code{cmd /C explorer}
#'
hsOpenWindowsExplorer <- function(
  startdir = tempdir(), use.shell.exec = ! .isNetworkPath(startdir)
)
{
  if (use.shell.exec) {

    shell.exec(file = startdir)

  } else {

    system(paste("cmd /C explorer", windowsPath(startdir)))
  }
}

# .isNetworkPath ---------------------------------------------------------------

#' Does the Path Represent a Network Path?
#'
#' @param x vector of character representing paths
#' 
#' @examples 
#' .isNetworkPath("//server/folder/file.txt")
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
#' 
windowsPath <- function(path)
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

.showCommand <- function(commandLine)
{
  cat(sprintf("Running command: >>>%s<<<\n", commandLine))
}

# hsSystem ---------------------------------------------------------------------

#' Wrapper around "system"
#'
#' @param commandLine character string passed to \code{system}
#' @param ... additional arguments passed to \code{system}
#' 
hsSystem <- function(commandLine, ...)
{
  .showCommand(commandLine)
  
  system(command = commandLine, ...)
}

# hsShell ----------------------------------------------------------------------

#' Wrapper around "shell"
#'
#' @param commandLine character string passed to \code{shell}
#' @param ... additional arguments passed to \code{shell}
#'
hsShell <- function(commandLine, ...)
{
  .showCommand(commandLine)
  
  shell(commandLine, ...)
}

# defaultWindowsProgramFolders -------------------------------------------------
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
mySystemTime <- function # mySystemTime
### mySystemTime
(
  FUN, args
) 
{
  stime <- system.time(returnValue <- do.call(FUN, args))
  cat(sprintf("Elapsed time: %0.2f s\n", stime["elapsed"]))
  returnValue
}

# runBatchfileInDirectory ------------------------------------------------------
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
cmdLinePath <- function # cmdLinePath
### cmdLinePath
(
  x
)
{
  hsQuoteChr(windowsPath(x), '"')
}

# .debugMessageIf --------------------------------------------------------------
.debugMessageIf <- function(dbg, ...)
{
  if (dbg) {
    cat(...)
  }  
}

# createDirAndReturnPath -------------------------------------------------------
createDirAndReturnPath <- function # create directory if it does not exist
### create directory if it does not exist
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
hsOpenWindowsExplorer <- function # open Windows Explorer
### open Windows Explorer
(
  startdir=tempdir(),
  ### directory to be opened in Windows Explorer
  use.shell.exec = TRUE
)
{
  if (use.shell.exec) {
    shell.exec(file=startdir)
  }
  else {
    hsShell(sprintf("explorer /e,/root,\"%s\"", windowsPath(startdir)))    
  }  
}

# windowsPath ------------------------------------------------------------------
windowsPath <- function # convert to MS Windows-compatible path
### create MS Windows-compatible path by substituting forward slashes with 
### backslashes
(
  path
) 
{
  gsub("/", "\\\\", path)  
}

# rStylePath -------------------------------------------------------------------
rStylePath <- function # R compatible file path
### R compatible file path with backslashes replaced with forward slashes
(
  path
) 
{
  gsub("\\\\", "/", path)
  ### path in which backslashes are replaced with forward slashes
}

# .showCommand -----------------------------------------------------------------
.showCommand <- function(commandLine)
{
  cat(sprintf("Running command: >>>%s<<<\n", commandLine))  
}

# hsSystem ---------------------------------------------------------------------
hsSystem <- function # wrapper around "system"
### wrapper around "system"
(commandLine, ...)
{
  .showCommand(commandLine)
  system(command=commandLine, ...)  
}

# hsShell ----------------------------------------------------------------------
hsShell <- function # wrapper around "shell"
### wrapper around "shell"
(commandLine, ...)
{
  .showCommand(commandLine)
  shell(commandLine, ...)  
}

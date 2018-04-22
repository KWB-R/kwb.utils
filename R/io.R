# headtail ---------------------------------------------------------------------

#' Print First and Last Rows of a Data Frame
#' 
#' Print the first and last rows of a data frame using head and tail, 
#' respectively. Print the number of omitted rows
#' 
#' @param x data frame
#' @param n total number of rows to be printed. 
#' @param pattern pattern given to \code{sprintf} containing a \code{\%d}
#'   placeholder to print the number of omitted rows
#'   
#' @return number of omitted rows, invisibly
#' 
#' @examples 
#' x <- data.frame(number = 1:26, letter = LETTERS)
#' headtail(x)
#' headtail(x, 10)
#' headtail(x, 16)
#' headtail(x[10:20, ], 10)
#' 
headtail <- function(x, n = 6, pattern = "[%d rows omitted]")
{
  if (! is.data.frame(x)) {
    
    stop("headtail() is currently only defined for data frames")
  }
  
  if (nrow(x) <= n) {
    
    print(x)
    
    n_omitted <- 0
    
  } else {
    
    n2 <- n %/% 2
    
    n_omitted <- nrow(x) - 2 * n2
    
    print(utils::head(x, n2))
    
    cat(sprintf(pattern, n_omitted), "\n")
    
    print(utils::tail(x, n2))
  }
  
  invisible(n_omitted)
}

# readPackageFile --------------------------------------------------------------

#' Read File from Package's extdata Folder
#' 
#' @param file name of file (without path)
#' @param package name of the package from which to read the file
#' @param stringsAsFactors passed to \code{\link[utils]{read.csv}} (default:
#'   \code{FALSE})
#' @param \dots further arguments passed to \code{\link[utils]{read.csv}}
#' 
#' @return result of reading \code{file} with \code{\link[utils]{read.csv}}
#' 
readPackageFile <- function(file, package, stringsAsFactors = FALSE, ...)
{
  file <- safePath(system.file("extdata", package = package), file)

  utils::read.csv(file = file, stringsAsFactors = stringsAsFactors, ...)
}

# getNamesOfObjectsInRDataFiles ------------------------------------------------

#' Deprecated. Use \code{\link{listObjects}} instead.
#'  
#' @param files.rdata vector of full paths to .RData files
#' 
getNamesOfObjectsInRDataFiles <- function(files.rdata)
{
  .warningDeprecated("getNamesOfObjectsInRDataFiles", "listObjects")
  
  listObjects(files.rdata)
}

# listObjects ------------------------------------------------------------------

#' Get Names of Objects in .RData files
#' 
#' @param files vector of full paths to .RData files
#' 
#' @examples 
#' ## Not run
#'   
#' ## Search for available .RData files below "searchdir"
#' #searchdir <- "//poseidon/projekte$/SUW_Department/Projects/SEMA/WP/20_Braunschweig"
#' #files <- dir(searchdir, pattern = "\\\\.RData$", recursive = TRUE, full.names = TRUE)
#'   
#' ## Get the names of the objects in the .RData files
#' #objectsInFiles <- listObjects(files)
#'   
#' ## Which file contains the object "DataQ"?
#' #dataQ.found <- sapply(objectsInFiles, function(x) {"DataQ" %in% x})
#'   
#' #cat("DataQ was found in the following files:",
#' #    paste(files[dataQ.found], collapse = "\n  "))
#' 
listObjects <- function(files)
{
  # Create new environment into which the .RData files are to be loaded
  # temporarily
  testenvironment <- new.env(parent = .GlobalEnv)
  
  # Prepare result list
  objectsInFiles <- list()
  
  # Loop through .RData files
  for (i in seq_len(length(files))) {
    
    cat(sprintf(
      "Loading %d/%d: %s... ", i, length(files), basename(files[i])
    ))
    
    load(file = files[i], envir = testenvironment)
    
    cat("ok. ")
    
    objectnames <- ls(envir = testenvironment)
    
    objectsInFiles[[i]] <- objectnames
    
    cat(length(objectnames), "objects found. ")
    
    cat("Clearing workspace... ")
    
    rm(list = ls(envir = testenvironment), envir = testenvironment)
    
    cat("ok.\n")
  }
  
  # Delete the testenvironment
  rm("testenvironment")
  
  # Return the list of object names
  structure(objectsInFiles, files = files)
}

# getObjectFromRDataFile -------------------------------------------------------

#' Deprecated. Please use \code{\link{loadObject}} instead.
#' 
#' @param file path to .RData file
#' @param objectname name of object to be loaded
#' @param dbg if \code{TRUE} a message about which object is loaded from which 
#'   file is shown
#' @return R object as specified in \emph{objectname}. If an object of that name does
#'   not exist in the .RData file an error is thrown
#' 
getObjectFromRDataFile <- function(file, objectname = NULL, dbg = TRUE)
{
  .warningDeprecated("getObjectFromRDataFile", "loadObject")
  
  loadObject(file, objectname, dbg)
}  

# .warningDeprecated -----------------------------------------------------------
.warningDeprecated <- function(old_name, new_name)
{
  warning(
    "The function ", old_name, "() is deprecated. Please use ", new_name, 
    "() instead.", call. = FALSE
  )
}

# loadObject -------------------------------------------------------------------

#' Load R object from .RData file
#' 
#' Load an R object of given name from a .RData file
#' 
#' @param file path to .RData file
#' @param objectname name of object to be loaded
#' @param dbg if \code{TRUE} a message about which object is loaded from which 
#'   file is shown
#' @return R object as specified in \emph{objectname}. If an object of that name does
#'   not exist in the .RData file an error is thrown
#' 
loadObject <- function(file, objectname = NULL, dbg = TRUE)
{
  envir <- new.env()

  catIf(dbg, sprintf("Loading '%s' from '%s'... ", objectname, file))
  
  load(safePath(file), envir = envir)

  # Get the names of the contained objects
  objectnames <- ls(envir = envir)

  if (is.null(objectname) || (! objectname %in% objectnames)) {

    message_1 <- if (is.null(objectname)) {
      
      "Please give an 'objectname'. "
      
    } else {
      
      sprintf("Object '%s' not found. ", objectname)
    }

    message_2 <- sprintf(
      "Available objects in '%s':\n%s", file, stringList(objectnames)
    )

    stop(message_1, message_2, call. = FALSE)
  }

  catIf(dbg, "ok.\n")

  # Return the object
  get(objectname, envir = envir)
}

# catLines ---------------------------------------------------------------------

#' Print Character Vector to the Console
#' 
#' Call cat on character vector, pasted with collapse = <new line>
#' 
#' @param x vector of character representing text lines to be printed
#' 
catLines <- function(x)
{
  cat(paste0(paste0(x, collapse = "\n"), "\n"))
}

# .logstart --------------------------------------------------------------------

.logstart <- function(dbg = TRUE, ...)
{
  catIf(dbg, "***", ..., "... ")
}

# .logok -----------------------------------------------------------------------

.logok <- function(dbg = TRUE)
{
  catIf(dbg, "*** ok.\n")
}

# .log -------------------------------------------------------------------------

.log <- function(...)
{
  cat("***", ...)
}

# .logline ---------------------------------------------------------------------

.logline <- function(...)
{
  cat("***", ..., "\n")
}

# catIf ------------------------------------------------------------------------

#' Call cat If Condition Is Met
#' 
#' @param condition if TRUE, cat is called, else not
#' @param \dots arguments passed to cat
#' 
catIf <- function(condition, ...)
{
  if (condition) {
    
    cat(...)
  }
}

# printIf ----------------------------------------------------------------------

#' Call Print If Condition Is Met
#' 
#' @param condition if TRUE, print is called, else not
#' @param x object to be printed
#' @param caption optional. Caption line to be printed with cat before printing
#'   \emph{x}
#' 
printIf <- function(condition, x, caption = deparse(substitute(x)))
{
  if (condition) {
    
    catIf(caption != "", paste0(caption, ":\n"))
    
    print(x)
  }
}

# clearConsole -----------------------------------------------------------------

#' Clear the R Console
#' 
clearConsole <- function()
{
  cat("\014\n")
}

# containsNulString ------------------------------------------------------------

#' Check for nul String in File
#' 
#' @param filepath full path to file to be checked
#' 
#' @return \code{TRUE} if first two bytes of file are \code{<FF><FE>}, else 
#'   \code{FALSE}
#' 
containsNulString <- function(filepath)
{
  x <- readBin(filepath, "raw", 2)
  
  x[1] == as.raw(0xff) && x[2] == as.raw(0xfe)
}

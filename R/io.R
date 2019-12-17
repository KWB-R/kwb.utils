# catAndRun --------------------------------------------------------------------

#' Print Debug Messages Before and After Running Code
#'
#' @param messageText text to be printed before running the code
#' @param expr expressions to be run. Enclose more than one expression in
#'   curly braces
#' @param newLine integer controlling new lines. 0: no extra new line, 1:
#'   new line after \code{messageText}, 2: new line after "ok.", 3: new line
#'   after both, \code{messageText} and "ok."
#' @param dbg logical. If \code{FALSE}, output is suppressed.
#' @param log_time logical. If \code{TRUE}, the time elapsed during the 
#'   evaluation of \code{expr} is printed.
#' @return This function returns the evaluation of \code{expr}. The result is
#'   returned invisibly so that the result of an assignment as the last 
#'   expression in \code{exprt} does not show up on the console.
#' @export
#' @examples
#' for (newLine in 0:3) {
#'
#'   catAndRun("work hard", newLine = newLine, {
#'     cat("hard\nworking\n")
#'   })
#'
#'   cat("here.\n\n")
#' }
#' 
catAndRun <- function(
  messageText = "Running code", expr, newLine = 2L, dbg = TRUE, log_time = TRUE
)
{
  catIf(dbg, messageText, "... ")
  
  catNewLineIf(dbg && bitwAnd(newLine, 1))
  
  start_time <- Sys.time()
  
  result <- eval(expr, envir = -1)

  catIf(dbg, "ok. ")
  
  catIf(dbg && log_time, sprintf("(%0.2fs) ", Sys.time() - start_time))
  
  catNewLineIf(dbg && bitwAnd(newLine, 2))
  
  invisible(result)
}

# catIf ------------------------------------------------------------------------

#' Call cat If Condition Is Met
#' 
#' @param condition if TRUE, cat is called, else not
#' @param \dots arguments passed to cat
#' @export
#' 
catIf <- function(condition, ...)
{
  if (condition) {
    
    cat(...)
  }
}

# catLines ---------------------------------------------------------------------

#' Print Character Vector to the Console
#' 
#' Call cat on character vector, pasted with collapse = <new line>
#' 
#' @param x vector of character representing text lines to be printed
#' @export
#' 
catLines <- function(x)
{
  cat(paste0(paste0(x, collapse = "\n"), "\n"))
}

# catNewLineIf -----------------------------------------------------------------

#' Print New Line Character to the Console if Condition is Met
#'
#' @param condition if \code{TRUE} the new line is printed else not
#' @return Returns the condition, invisibly so that it can be reused
#' @export
#' 
catNewLineIf <- function(condition)
{
  catIf(condition, "\n")
  
  invisible(condition)
}

# clearConsole -----------------------------------------------------------------

#' Clear the R Console
#' 
#' @export
#' 
clearConsole <- function()
{
  cat("\014\n")
}

# containsNulString ------------------------------------------------------------

#' Check for nul String in File
#' 
#' @param filepath full path to file to be checked
#' @return \code{TRUE} if first two bytes of file are \code{<FF><FE>}, else 
#'   \code{FALSE}
#' @export
#' 
containsNulString <- function(filepath)
{
  x <- readBin(filepath, "raw", 2)
  
  x[1] == as.raw(0xff) && x[2] == as.raw(0xfe)
}

# getNamesOfObjectsInRDataFiles ------------------------------------------------

#' Deprecated. Use \code{\link{listObjects}} instead.
#'  
#' @param files.rdata vector of full paths to .RData files
#' @export
#' 
getNamesOfObjectsInRDataFiles <- function(files.rdata)
{
  warningDeprecated("getNamesOfObjectsInRDataFiles", "listObjects")
  
  listObjects(files.rdata)
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
#' @export
#' 
getObjectFromRDataFile <- function(file, objectname = NULL, dbg = TRUE)
{
  warningDeprecated("getObjectFromRDataFile", "loadObject")
  
  loadObject(file, objectname, dbg)
}  

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
#' @return number of omitted rows, invisibly
#' @export
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

# listObjects ------------------------------------------------------------------

#' Get Names of Objects in .RData files
#' 
#' @param files vector of full paths to .RData files
#' @export
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
#' @export
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

# printIf ----------------------------------------------------------------------

#' Call Print If Condition Is Met
#' 
#' @param condition if TRUE, print is called, else not
#' @param x object to be printed
#' @param caption optional. Caption line to be printed with cat before printing
#'   \emph{x}
#' @export
#' 
printIf <- function(condition, x, caption = deparse(substitute(x)))
{
  if (condition) {
    
    catIf(caption != "", paste0(caption, ":\n"))
    
    print(x)
  }
}

# readPackageFile --------------------------------------------------------------

#' Read File from Package's extdata Folder
#' 
#' @param file name of file (without path)
#' @param package name of the package from which to read the file
#' @param stringsAsFactors passed to \code{utils::read.csv} (default:
#'   \code{FALSE})
#' @param \dots further arguments passed to \code{utils::read.csv}
#' 
#' @return result of reading \code{file} with \code{utils::read.csv}
#' @export
#' 
readPackageFile <- function(file, package, stringsAsFactors = FALSE, ...)
{
  file <- safePath(system.file("extdata", package = package), file)

  utils::read.csv(file = file, stringsAsFactors = stringsAsFactors, ...)
}

# warningDeprecated ------------------------------------------------------------

#' Create Warning About a Deprecated Function
#' 
#' @param old_name name of deprecated function
#' @param new_name name of new function to be used instead
#' @export
#' @examples
#' warningDeprecated("old_function()", "new_function()")
#' 
warningDeprecated <- function(old_name, new_name)
{
  warning(
    "The function ", old_name, "() is deprecated. Please use ", new_name, 
    "() instead.", call. = FALSE
  )
}

# writeText --------------------------------------------------------------------

#' Write Text Lines to a File
#'
#' Write text to a file using \code{\link{writeLines}} and output a debug
#' message by default
#'
#' @param x vector of character representing the lines to be written to
#'   \code{file}, passed to \code{\link{writeLines}}
#' @param file path to file to be written, passed to \code{\link{writeLines}}
#' @param type character string to be included in the debug message:
#'   "Writing <type>'file-path' ..."
#' @param dbg if \code{TRUE}, debug messages are shown
#' @param \dots further arguments passed to \code{\link{writeLines}}
#' @return This function invisibly returns the path to the output \code{file}.
#' @export
#' @examples
#' # Define text to be written to file
#' x <- c("Hello", "world")
#' 
#' # Write text to a temporary file and catch the file path
#' file <- writeText(x, tempfile(fileext = ".txt"))
#'
#' # Make the debug message more informative
#' writeText(x, file, type = "welcome file")
#'
#' # Read lines back and show on the console
#' catLines(readLines(file))
#'
writeText <- function(x, file, type = "", dbg = TRUE, ...)
{
  catIf(dbg, sprintf(
    "Writing %s%s'%s' ... ", type, ifelse(type == "", "", " "), file
  ))
  
  writeLines(x, file, ...)
  
  catIf(dbg, "ok.\n")
  
  invisible(file)
}

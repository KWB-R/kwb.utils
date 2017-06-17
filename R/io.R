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
#' @examples 
#' x <- data.frame(number = 1:26, letter = LETTERS)
#' headtail(x)
#' headtail(x, 10)
#' headtail(x, 16)
#' headtail(x[10:20, ], 10)
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
    
    print(head(x, n2))
    cat(sprintf(pattern, n_omitted), "\n")
    print(tail(x, n2))
  }
  
  invisible(n_omitted)
}

# readPackageFile --------------------------------------------------------------

#' read file from package's extdata folder
#' 
#' read file from package's "extdata" folder
#' 
#' @param file name of file (without path)
#' @param package name of the package from which to read the file
#' @param stringsAsFactors passed to \code{\link[utils]{read.csv}} (default: \code{FALSE})
#' @param \dots further arguments passed to \code{\link[utils]{read.csv}}
#' 
#' @return result of reading \code{file} with \code{\link[utils]{read.csv}}
#' 
readPackageFile <- function # read file from package's extdata folder
### read file from package's "extdata" folder
(
  file,
  ### name of file (without path)
  package,
  ### name of the package from which to read the file
  stringsAsFactors = FALSE,
  ### passed to \code{\link[utils]{read.csv}} (default: \code{FALSE})
  ...
  ### further arguments passed to \code{\link[utils]{read.csv}}
)
{
  file <- safePath(system.file("extdata", package = package), file)

  utils::read.csv(file = file, stringsAsFactors = stringsAsFactors, ...)
  ### result of reading \code{file} with \code{\link[utils]{read.csv}}
}

# getObjectFromRDataFile -------------------------------------------------------

#' load R object from .RData file
#' 
#' load R object from a .RData file
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
  envir <- new.env()

  catIf(dbg, sprintf("Loading '%s' from '%s'... ", objectname, file))
  load(safePath(file), envir = envir)

  # Get the names of the contained objects
  objectnames <- ls(envir = envir)

  if (is.null(objectname) || (! objectname %in% objectnames)) {

    messageText <- if (is.null(objectname)) {
      "Please give an 'objectname'."
    } else {
      sprintf("Object '%s' not found.", objectname)
    }

    messageText <- sprintf(
      "%s Available objects in '%s':\n%s",
      messageText, file, collapsed(hsQuoteChr(objectnames), ", ")
    )

    stop(messageText, call. = FALSE)
  }

  catIf(dbg, "ok.\n")

  # Return the object
  get(objectname, envir = envir)

  ### R object as specified in \emph{objectname}. If an object of that name does
  ### not exist in the .RData file an error is thrown
}

# catLines ---------------------------------------------------------------------

#' print character vector to the console
#' 
#' call cat on character vector, pasted with collapse = <new line>
#' 
#' @param x vector of character representing text lines to be printed
catLines <- function # print character vector to the console
### call cat on character vector, pasted with collapse = <new line>
(x)
{
  cat(paste0(paste0(x, collapse = "\n"), "\n"))
}

# .logstart --------------------------------------------------------------------

#'  logstart
#' 
#' 
.logstart <- function(dbg = TRUE, ...)
{
  catIf(dbg, "***", ..., "... ")
}

# .logok -----------------------------------------------------------------------

#'  logok
#' 
#' 
.logok <- function(dbg = TRUE)
{
  catIf(dbg, "*** ok.\n")
}

# .log -------------------------------------------------------------------------

#'  log
#' 
#' 
.log <- function(...)
{
  cat("***", ...)
}

# .logline ---------------------------------------------------------------------

#'  logline
#' 
#' 
.logline <- function(...)
{
  cat("***", ..., "\n")
}

# catIf ------------------------------------------------------------------------

#' call cat if condition is met
#' 
#' call cat if condition is met
#' 
#' @param condition if TRUE, cat is called, else not
#' @param \dots arguments passed to cat
#' 
catIf <- function # call cat if condition is met
### call cat if condition is met
(
  condition,
  ### if TRUE, cat is called, else not
  ...
  ### arguments passed to cat
)
{
  if (condition) {
    cat(...)
  }
}

# printIf ----------------------------------------------------------------------

#' call print if condition is met
#' 
#' call print if condition is met
#' 
#' @param condition if TRUE, print is called, else not
#' @param x object to be printed
#' @param caption optional. Caption line to be printed with cat before printing \emph{x}
#' 
printIf <- function # call print if condition is met
### call print if condition is met
(
  condition,
  ### if TRUE, print is called, else not
  x,
  ### object to be printed
  caption = deparse(substitute(x))
  ### optional. Caption line to be printed with cat before printing \emph{x}
)
{
  if (condition) {
    catIf(caption != "", paste0(caption, ":\n"))
    print(x)
  }
}

# clearConsole -----------------------------------------------------------------

#' Clear the R Console
#' 
#' Clear the R Console
#' 
clearConsole <- function # Clear the R Console
### Clear the R Console
(
)
{
  cat("\014\n")
}

# containsNulString ------------------------------------------------------------

#' Check for nul String in File
#' 
#' check for nul string in file
#' 
#' @param filepath full path to file to be checked
#' @return \code{TRUE} if first two bytes of file are \code{<FF><FE>}, else 
#'   \code{FALSE}
#' 
containsNulString <- function # containsNulString
### check for nul string in file
(
  filepath
)
{
  x <- readBin(filepath, "raw", 2)
  x[1] == as.raw(0xff) && x[2] == as.raw(0xfe)
  ### TRUE if first two bytes of file are FF FE, else FALSE
}

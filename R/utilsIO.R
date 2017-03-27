# printLines -------------------------------------------------------------------
printLines <- function # printLines
### printLines
(x)
{
  cat(paste(x, collapse="\n"))
}

# lastElement ------------------------------------------------------------------
lastElement <- function # lastElement
### lastElement
(x)
{
  x[length(x)]
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
printIf <- function # call print if condition is met
### call print if condition is met
(
  condition,
  ### if TRUE, print is called, else not
  x,
  ### object to be printed
  caption = ""
  ### optional. Caption line to be printed with cat before printing \emph{x}
) 
{
  if (condition) {
    catIf(caption != "", paste0(caption, ":\n"))
    print(x)
  }
}

# clearConsole -----------------------------------------------------------------
clearConsole <- function # Clear the R Console
### Clear the R Console
(
)
{
  cat("\014\n")
}

# containsNulString ------------------------------------------------------------
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


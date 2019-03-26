# .log -------------------------------------------------------------------------

#' Write Log Message to Console
#' 
#' @param \dots parts of the message that will be passed to \code{\link{cat}}
#' @export
#' 
.log <- function(...)
{
  cat("***", ...)
}

# .logline ---------------------------------------------------------------------

#' Write Log Message and New Line Character to Console
#' 
#' @param \dots parts of the message that will be passed to \code{\link{cat}}
#' @export
#' 
.logline <- function(...)
{
  cat("***", ..., "\n")
}

# .logok -----------------------------------------------------------------------

#' Write "ok" and New Line Character to Console
#' 
#' @param dbg logical to switch logging on (\code{dbg = TRUE}) or off (\code{dbg
#'   = FALSE})
#' @export
#' 
.logok <- function(dbg = TRUE)
{
  catIf(dbg, "*** ok.\n")
}

# .logstart --------------------------------------------------------------------

#' Write Log Message to Console if in Debugging Mode
#' 
#' @param dbg logical to switch logging on (\code{dbg = TRUE}) or off (\code{dbg
#'   = FALSE})
#' @param \dots parts of the message that will be passed to \code{\link{catIf}}
#' @export
#' 
.logstart <- function(dbg = TRUE, ...)
{
  catIf(dbg, "***", ..., "... ")
}

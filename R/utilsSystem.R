# safePath ---------------------------------------------------------------------

#' stop if path does not exit
#' 
#' stop if path does not exit and return the path
#' 
#' @param \dots character vectors passed to \code{file.path}
#' 
#' @return path as given in \code{path}
#' 
safePath <- function # stop if path does not exit
### stop if path does not exit and return the path
(
  ...
  ### character vectors passed to \code{file.path}
)
{
  file <- file.path(...)

  if (! file.exists(file)) {

    dirpath <- dirname(file)

    if (! file.exists(dirpath)) {
      
      stop("No such directory: ", dirpath, call. = FALSE)
      
    } else {
      
      stop("No such file: ", hsQuoteChr(basename(file)), " in ", 
           hsQuoteChr(dirpath), "\nAvailable files: ", stringList(dir(dirpath)), 
           call. = FALSE)
    }
  }

  file
  ### path as given in \code{path}
}

# .OStype ----------------------------------------------------------------------

#' see tools:::.OStype
#' 
#' This is a copy of tools:::.OStype. It is redefined here so that it can
#'   be used within this package. R CMD build would complain if I used
#'   tools:::.OStype!
#' 
.OStype <- function # see tools:::.OStype
### This is a copy of tools:::.OStype. It is redefined here so that it can
### be used within this package. R CMD build would complain if I used
### tools:::.OStype!
()
{
  OS <- Sys.getenv("R_OSTYPE")
  if (nzchar(OS))
    OS
  else .Platform$OS.type
}

# desktop ----------------------------------------------------------------------

#' path to your desktop
#' 
#' get the path to your desktop
#' 
#' @return character string representing the path to your desktop
#' 
desktop <- function # path to your desktop
### get the path to your desktop
()
{
  osType <- .OStype()

  desktops <- c(
    windows = "C:/Users/<user>/Desktop",
    unix = "/home/<user>/Desktop"
  )

  if (isNaOrEmpty(desktops[osType])) {
    stop("I do not know where the desktop is in: ", osType)
  }

  hsResolve("desktop", dict = list(desktop = desktops[osType]), user = user())
  ### character string representing the path to your desktop
}

# user -------------------------------------------------------------------------

#' name of the current user
#' 
#' Get the name of the current user from the environment variables
#' 
#' @return character string represenging the name of the current user
#' 
user <- function # name of the current user
### Get the name of the current user from the environment variables
()
{
  osType <- .OStype()

  user <- Sys.getenv(c(windows = "USERNAME", unix = "USER")[osType])

  if (isNaOrEmpty(user)) {
    stop("I do not know how to get the user name in: ", osType)
  }

  user
  ### character string represenging the name of the current user
}

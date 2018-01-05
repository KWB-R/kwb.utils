#' Get file path of user's home directory
#' @return path of user's home directory on windows (full path 
#' home directory path but without '/Documents') or linux

get_homedir <- function()
{
  if (.Platform$OS.type == "windows") {
   homedir <-  gsub("/Documents", "", Sys.getenv("HOME"))
  } else {
   homedir <-  Sys.getenv("HOME")
  }
  
return(homedir)
}


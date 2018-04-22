#' Get File Path of User's Home Directory
#' 
#' @return path of user's home directory on windows (full path home directory
#'   path but without '/Documents') or linux
#'   
get_homedir <- function()
{
  if (.Platform$OS.type == "windows") {
    
   gsub("/Documents", "", Sys.getenv("HOME"))
    
  } else {
    
   Sys.getenv("HOME")
  }
}

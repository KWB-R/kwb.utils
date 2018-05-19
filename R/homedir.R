#' Get File Path of User's Home Directory
#' 
#' @return path of user's home directory on windows (full path home directory
#'   path but without '/Documents') or linux
#'   
get_homedir <- function()
{
  home <- Sys.getenv("HOME")
  
  if (.Platform$OS.type == "windows") {
    
   gsub("/Documents", "", home)
    
  } else {
    
   home
  }
}

#' Get File Path of User's Home Directory
#' 
#' @param osType Optional. Type of operating system, one of \code{"unix"},
#'   \code{"windows"}
#' 
#' @return path of user's home directory on windows (full path home directory
#'   path but without '/Documents') or linux
#'
get_homedir <- function(osType = .OStype())
{
  home <- Sys.getenv("HOME")
  
  if (osType == "windows") {
    
   gsub("/Documents", "", home)
    
  } else {
    
   home
  }
}

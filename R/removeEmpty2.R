#' Remove Empty Elements from Vector of Character
#' 
#' @param x vector of character
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) a message about the 
#'   number of removed elements is printed
#' @export
#' @examples
#' removeEmpty2(c("a", "", "c"))
#' removeEmpty2(c("a", "", "c", "", "e"), dbg = TRUE)
removeEmpty2 <- function(x, dbg = FALSE)
{
  isEmpty <- (x == "")
  
  if (!any(isEmpty)) {
    return(x)
  }
  
  catIf(dbg, sum(isEmpty), "elements removed.\n")
  
  x[!isEmpty]
}

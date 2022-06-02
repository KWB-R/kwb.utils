removeEmpty2 <- function(x, dbg = FALSE)
{
  isEmpty <- (x == "")
  
  if (!any(isEmpty)) {
    return(x)
  }
  
  catIf(dbg, sum(isEmpty), "elements removed.\n")
  
  x[!isEmpty]
}

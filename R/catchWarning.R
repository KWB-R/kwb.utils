#' Return Warning Message in Attribute
#' 
#' This function evaluates an expression and returns, if warnings occurred, the
#' warning message in the attribute "warning_message" of the returned object.
#' 
#' @param expr expression to be evaluated
#' @param dbg if \code{TRUE} (the default) the warning text is also printed on
#'   the console, otherwise the warning message is suppressed.
#' @export
#' @examples 
#' catchWarning(as.numeric("1.23"))
#' result <- catchWarning(as.numeric("x"))
#' result <- catchWarning(as.numeric("x"), dbg = FALSE)
#' str(result)
catchWarning <- function(expr, dbg = TRUE)
{
  suppressWarnings(result <- tryCatch(
    eval(expr),  
    warning = function(w) {
      kwb.utils::printIf(dbg, w$message, caption = "There was a warning")
      structure(eval(expr), warning_message = w$message)
    }
  ))
  
  result
}

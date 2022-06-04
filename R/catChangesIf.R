#' Print Differences Between Two Vectors if Condition is Met
#' 
#' @param dbg if \code{TRUE} \code{\link{catChanges}} is called on \code{x} and
#'   \code{y}
#' @param x vector
#' @param y vector
#' @export
catChangesIf <- function(dbg, x, y)
{
  if (dbg) {
    catChanges(x, y)
  }
}

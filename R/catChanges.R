#' Print Differences Between Two Vectors
#' 
#' @param x vector
#' @param y vector
#' @export
#' @examples 
#' catChanges(c(1, 2, 3), c(1, 22, 3))
catChanges <- function(x, y)
{
  differs <- x != y
  
  kwb.utils::catLines(sprintf("%s -> %s", x[differs], y[differs]))
}

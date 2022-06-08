#' Stop Function Exectution With Formatted Message 
#' 
#' @param x Error message, possibly containing percentage placeholders, passed
#'   as argument \code{fmt} to \code{\link{sprintf}}
#' @param \dots as many values as referenced with percentage placeholders in 
#'   the error message
#' @inheritParams base::stop
#' @export
#' @examples
#' try(stopFormatted(
#'   "Hi, %s, the program fails for the %d-th time.", "Dexter", 1000
#' ))
stopFormatted <- function(x, ..., call. = FALSE)
{
  stop(sprintf(x, ...), call. = call.)
}

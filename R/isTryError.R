#' Does an Object Inherit from "try-error"?
#'
#' @param x R object
#' @return logical of length one, TRUE if \code{x} inherits from "try-error",
#'   otherwise FALSE
#' @export
#' @examples
#' result <- try(stop("Stop!"), silent = TRUE)
#' isTryError(result) # TRUE
isTryError <- function(x)
{
  inherits(x, "try-error")
}

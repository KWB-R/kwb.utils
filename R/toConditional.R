# toConditional ----------------------------------------------------------------

#' Convert Function to Function that is Called if Condition is Met
#'
#' @param FUN a function
#' @export
#' @examples
#' square <- function(x) x^2
#' negate <- function(x) -x
#' `%>%` <- magrittr::`%>%`
#' do_square <- TRUE
#' do_negate <- TRUE
#' 10 %>%
#'   toConditional(square)(do_square) %>%
#'   toConditional(negate)(do_negate)
toConditional <- function(FUN)
{
  stopifnot(is.function(FUN))

  function(x, condition, ...) {

    stopifnot(is.logical(condition), length(condition) == 1L)

    if (! condition) {
      return(x)
    }

    FUN(x, ...)
  }
}

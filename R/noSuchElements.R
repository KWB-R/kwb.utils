#' Message on Missing Elements
#'
#' @param x name of element that was not found
#' @param available names of elements that are available
#' @param type type of element to appear in the message. Default: "element"
#' @param sorted logical. Whether or not to print the available elements in
#'   lexical order. Default: \code{TRUE}
#' @param suffix suffix to be appended to \code{type}. Can be used to
#'   distinguish between singular and plural form. Default: \code{""}
#' @export
#' @examples
#' cat(kwb.utils:::noSuchElements("x", LETTERS[1:3]))
#' cat(kwb.utils:::noSuchElements(c("x", "y"), LETTERS[1:3], suffix = "s"))
noSuchElements <- function(
  x, available, type = "element", sorted = TRUE, suffix = ""
)
{
  paste0(
    hintNoSuch(x, type, sorted, suffix),
    hintAvailable(available, type, sorted)
  )
}

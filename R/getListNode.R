#' Get List Element in Nested List Structure by Path
#' 
#' This function allows to access a list element of a nested list structure with
#' a "path". The path "a/b/c", for example, applied to a list L refers to list 
#' element L[["a"]][["b"]][["c"]] (same as L[[c("a", "b", "c")]]).
#' 
#' @param x a list
#' @param path "path" to list element in the form <key1>/<key2>/<key3>...
#' @export
#' @examples
#' L <- list(
#'   a1 = list(
#'     b1 = list(c = 1, d = 2, e = 3),
#'     b2 = list(c = list(c1 = 1, c2 = 2, c3 = 3))
#'   ),
#'   a2 = list(b3 = 22, b4 = 44)
#' )
#' 
#' getListNode(L, "a1/b2/c/c2")
#' getListNode(L, "a1/b2/c")
#' getListNode(L, "a2/b3")
#' 
getListNode <- function(x, path)
{
  stopifnot(is.list(x))
  
  parts <- strsplit(path, "/")[[1L]]
  
  Reduce(f = selectElements, x = parts, init = x)
}

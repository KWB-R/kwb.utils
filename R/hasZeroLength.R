#' Does an Object Have a Length of Zero?
#'
#' This is just a "shortcut" to "length(x) == 0L"
#' 
#' @param x R object that has a length attribute, e.g. vector or list
#' @return \code{TRUE} if \code{length(x) == 0L}, otherwise \code{FALSE}
#' @export
#' @examples
#' hasZeroLength(character()) # TRUE
#' 
#' hasZeroLength(list()) # TRUE
#' 
#' # Do not confuse, has nothing to do with the length of a string
#' # here: vector of length one
#' hasZeroLength("") # FALSE
#' 
#' # Remember that the length of a data frame is the number of its columns
#' hasZeroLength(data.frame(a = character())) # FALSE
#' 
hasZeroLength <- function(x)
{
  length(x) == 0L
}

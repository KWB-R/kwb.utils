#' Main Class of an Object
#' 
#' Returns the first element of what class(x) returns
#' 
#' @param x any R object
#' @return name of main class of \code{x} (vector of character of length one)
#' @export
#' @examples
#' class(as.POSIXct("2022-06-02"))
#' mainClass(as.POSIXct("2022-06-02"))
mainClass <- function(x)
{
  class(x)[1L]
}

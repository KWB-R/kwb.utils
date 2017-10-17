# enlargeVector ----------------------------------------------------------------

#' Enlarge a Vector to given Length
#' 
#' Enlarge a vector to the given length, filling with given element
#' 
#' @param x vector
#' @param length.out desired length of output vector
#' @param fill.with element to fill the vector up with (default: "")
#' 
#' @return \code{x}, filled up with \code{fill.with} to a final length of
#'   \code{length.out}
#' 
#' @examples 
#' kwb.utils::enlargeVector(1:5, 10, fill.with = 0) 
#' kwb.utils::enlargeVector(1:5, 10, fill.with = NA) 
#' kwb.utils::enlargeVector(c("a", "b", "c"), 10) 
#' kwb.utils::enlargeVector(c("a", "b", "c"), 10, fill.with = "?")
#' 
enlargeVector <- function(x, length.out, fill.with = "")
{
  N <- length(x)
  
  stopifnot (N <= length.out)
  
  c(x, rep(fill.with, length.out - N))
}

# recycle ----------------------------------------------------------------------

#' "Recycle" Vector to given Length
#' 
#' @param x vector to be "recycled"
#' @param n target length
#' 
recycle <- function(x, n)
{
  rep(x, length.out = n)
}

# firstElement -----------------------------------------------------------------

#' First Element
#' 
#' Returns the first element using the function head
#' 
#' @param x object
#' 
#' @return first element: x[1]
#' 
firstElement <- function(x)
{
  utils::head(x, 1)
}

# lastElement ------------------------------------------------------------------

#' Last Element
#' 
#' Returns the last element using the function tail
#' 
#' @param x object
#' 
#' @return last element: x[length(x)]
#' 
lastElement <- function(x)
{
  utils::tail(x, 1)
}

# getByPositiveOrNegativeIndex -------------------------------------------------

#' Get Vector Elements by positive or negative Index
#' 
#' Get element from vector, counting from head or tail
#' 
#' @param elements vector of elements
#' @param index positive or negative index(es) with absolute value between 1 and
#'   length(\emph{elements})
#'   
#' @return element(s) out of \emph{elements} corresponding to the index(es)
#'   given in \emph{index}
#' 
getByPositiveOrNegativeIndex <- function(elements, index)
{
  n <- length(elements)
  
  if (! all(inRange(values = abs(index), min.value = 1, max.value = n))) {
    
    stop("There are invalid indices. The maximum allowed (absolute) value of ",
         "an index is ", n, " (number of elements).")
  }
  
  elements[toPositiveIndices(index, n = n)]
}

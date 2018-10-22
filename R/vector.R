# combineAlternatingly ---------------------------------------------------------

#' Combine Two Vectors Alternatingly
#' 
#' @param a first vector
#' @param b second vector
#' 
#' @return vector \code{x} with all \code{x[c(1, 3, 5, ...)] == a[c(1, 2, 3,
#'   ...)]} and all \code{x[c(2, 4, 6, ...)] == b[c(1, 2, 3, ...)]}
#' 
#' @examples
#' a <- paste0("a", 1:5)
#' b <- paste0("b", 1:5)
#' 
#' combineAlternatingly(a, b)
#' combineAlternatingly(b, a)
#' 
#' combineAlternatingly(list(a = 1, b = 2), list(c = 3, d = 4))
#' 
combineAlternatingly <- function(a, b)
{
  stopifnot(is.vector(a), is.vector(b))
  stopifnot(length(a) == length(b))
  stopifnot(identical(class(a), class(b)))
  
  result <- vector()
  
  indices <- seq_len(2 * length(a))
  
  odd_indices <- kwb.utils::getOddNumbers(indices)
  even_indices <- kwb.utils::getEvenNumbers(indices)
  
  result[odd_indices] <- a
  result[even_indices] <- b
  
  names(result)[odd_indices] <- names(a)
  names(result)[even_indices] <- names(b)
  
  result
}

# enlargeVector ----------------------------------------------------------------

#' Enlarge a Vector to Given Length
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

#' Get Vector Elements by Positive or Negative Index
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

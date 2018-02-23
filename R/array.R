# dropDim ----------------------------------------------------------------------
#'
#' Drop Array Dimension(s) of Length One
#'
#' @param x an array
#' @param dimension number(s) of dimension(s) of length one to be removed
#'
#' @return array with dimensions of which the numbers are given in
#' \code{dimension} removed
#'
#' @examples
#' # Define an array of two matrices
#' A <- array(
#' 1:8, dim = c(2, 2, 2), dimnames = list(
#'     paste0("x", 1:2), paste0("y", 1:2), paste0("z", 1:2))
#' )
#'
#' # The aim is to select the first column of the first matrix with
#' # the matrix structure being kept. This cannot be done with the
#' # standard "[" operator. It has indeed a "drop" argument but this
#' # acts on all dimensions:
#'
#' # By default, drop is TRUE. The result is a named vector
#' A[, 1, 1]
#'
#' # With drop = FALSE we get a 3D-array again and not a matrix
#' A[, 1, 1, drop = FALSE]
#'
#' # Use dropDim to remove the third dimension of an array that
#' # has already one dimension of length one
#' dropDim(A[, 1, 1, drop = FALSE], dimension = 3)
#'
dropDim <- function(x, dimension = which(dim(x) == 1))
{
  stopifnot(is.array(x), is.numeric(dimension), all(dim(x)[dimension] == 1L))

  dim_keep <- setdiff(seq_len(length(dim(x))), dimension)

  array(x, dim = dim(x)[dim_keep], dimnames = dimnames(x)[dim_keep])
}

#' Split Array Along a Dimension
#' 
#' Split an array along its n-th dimension. The implementation was found here:
#' https://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
#' 
#' @param a an array
#' @param n number of the dimension along which to split the array
#' 
#' @return array of one dimension less than \code{a}
#' 
#' # Define an array
#' A <- array(1:8, dim = c(2, 2, 2), dimnames = list(
#'   paste0("x", 1:2), paste0("y", 1:2), paste0("z", 1:2)
#' ))
#' 
#' splitAlongDim(A, 1)
#' splitAlongDim(A, 2)
#' splitAlongDim(A, 3)
#'
splitAlongDim <- function(a, n)
{
  stopifnot(is.array(a), n <= length(dim(a)))
  
  stats::setNames(
    lapply(
      split(a, arrayInd(seq_along(a), dim(a))[, n]),
      array, 
      dim = dim(a)[-n], 
      dimnames(a)[-n]
    ),
    dimnames(a)[[n]]
  )
}

# mergeNamedArrays -------------------------------------------------------------

#' Merge List of Named Arrays
#'
#' @param x list of arrays of the same dimension
#' @param check_dim logical. If \code{TRUE}, it is checked whether the source
#'   dimension names are available in the target dimension names
#' @export
#' @examples
#' a1 <- array(
#'   1:12,
#'   dim = c(2, 4, 2),
#'   dimnames = list(paste0("x", 1:2), paste0("y", 1:4), paste0("z", 1:2))
#' )
#'
#' a2 <- array(
#'   11:16,
#'   dim = c(1, 3, 2),
#'   dimnames = list("x3", paste0("y", 2:4), paste0("z", 1:2))
#' )
#'
#' mergeNamedArrays(list(a1, a2))
#'
mergeNamedArrays <- function(x, check_dim = TRUE)
{
  # Stop if x is not a list of arrays
  stopifnot(is.list(x), all(sapply(x, is.array)))

  # Get a list of dimension lists
  dimnames_list <- lapply(x, dimnames)

  if (any(sapply(dimnames_list, is.null))) {

    stop("All arrays must have dimension names", call. = FALSE)
  }

  # Stop if not all dimensions are of the same length
  stopifnot(allAreEqual(sapply(dimnames_list, length)))

  # Merge the dimension lists
  dim_names <- lapply(seq_along(dimnames_list[[1]]), function(i) {

    unique(unlist(lapply(dimnames_list, "[[", i)))
  })

  # Copy the names of the dimensions, if they are named
  names(dim_names) <- names(dimnames_list[[1]])

  # Create the target array
  target <- array(dim = sapply(dim_names, length), dimnames = dim_names)

  # Call mergeArray successively for each array in x
  Reduce(x = x, init = target, f = function(b, a) {

    dimnames_a <- dimnames(a)

    if (check_dim) {

      .checkDimensions(dimnames_a, dimnames_b = dim_names)
    }

    b[as.matrix(do.call(expand.grid, dimnames_a))] <- a

    b
  })
}

# .checkDimensions -------------------------------------------------------------

.checkDimensions <- function(dimnames_a, dimnames_b)
{
  # Define helper functions
  get_lengths <- function(x) sapply(x, length)
  print_with_caption <- function(x) printIf(TRUE, x)

  dim_a <- get_lengths(dimnames_a)
  dim_b <- get_lengths(dimnames_b)

  stopifnot(length(dim_a) == length(dim_b))

  if (! all(dim_a <= dim_b)) {

    print_with_caption(dim_a)
    print_with_caption(dim_b)

    stop("Target dimensions are too small!")
  }

  for (i in seq_along(dim_a)) {

    a_in_b <- dimnames_a[[i]] %in% dimnames_b[[i]]

    if (! all(a_in_b)) {

      stop(
        "There are labels in a that are not in b in dimension ", i, ", e.g.:\n",
        stringList(utils::head(dimnames_a[[i]][! a_in_b]))
      )
    }
  }
}


# dropDim ----------------------------------------------------------------------

#' Drop Array Dimension(s) of Length One
#'
#' @param x an array
#' @param dimension number(s) of dimension(s) of length one to be removed
#' @return array with dimensions of which the numbers are given in
#'   \code{dimension} removed
#' @export
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

# splitAlongDim ----------------------------------------------------------------

#' Split Array Along a Dimension
#'
#' Split an array along its n-th dimension. The implementation was found here:
#' https://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
#'
#' @param a an array
#' @param n number of the dimension along which to split the array
#' @return array of one dimension less than \code{a}
#' @export
#' @examples
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

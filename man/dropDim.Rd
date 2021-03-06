% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/array.R
\name{dropDim}
\alias{dropDim}
\title{Drop Array Dimension(s) of Length One}
\usage{
dropDim(x, dimension = which(dim(x) == 1))
}
\arguments{
\item{x}{an array}

\item{dimension}{number(s) of dimension(s) of length one to be removed}
}
\value{
array with dimensions of which the numbers are given in
  \code{dimension} removed
}
\description{
Drop Array Dimension(s) of Length One
}
\examples{
# Define an array of two matrices
A <- array(
1:8, dim = c(2, 2, 2), dimnames = list(
    paste0("x", 1:2), paste0("y", 1:2), paste0("z", 1:2))
)

# The aim is to select the first column of the first matrix with
# the matrix structure being kept. This cannot be done with the
# standard "[" operator. It has indeed a "drop" argument but this
# acts on all dimensions:

# By default, drop is TRUE. The result is a named vector
A[, 1, 1]

# With drop = FALSE we get a 3D-array again and not a matrix
A[, 1, 1, drop = FALSE]

# Use dropDim to remove the third dimension of an array that
# has already one dimension of length one
dropDim(A[, 1, 1, drop = FALSE], dimension = 3)

}

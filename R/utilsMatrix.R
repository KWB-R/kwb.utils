# createMatrix -----------------------------------------------------------------
createMatrix <- structure(
  function # matrix with row and column names
### Create a matrix by giving row and column names and with all elements being
### set to a default value
(
  rowNames,
  ### character vector of row names to be given to the matrix
  colNames = rowNames,
  ### character vector of column names to be given to the matrix
  value = 0
  ### value to be given to each matrix element
)
{
  stopifnot(is.character(rowNames))
  stopifnot(is.character(colNames))
  
  nrows <- length(rowNames)
  
  matrix(
    data = rep(value, times = nrows * length(colNames)), 
    nrow = nrows,
    dimnames = list(rowNames, colNames)
  )
  
  ### matrix with \code{rowNames} as row names and \code{colNames} as column
  ### names, filled with \emph{value} at each position
}, ex = function() {
  ## Initialise a matrix with rows A to E and columns x to z of value -1
  createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)
  
  ## By default the column names are assumed to be equal to the row names
  createMatrix(c("A", "B", "C"))
  
  ## Initialise a square matrix with NA
  createMatrix(c("A", "B", "C"), value = NA)
})

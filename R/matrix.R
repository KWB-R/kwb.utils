# linearCombination ------------------------------------------------------------

#' Linear Combination of a Matrix
#' 
#' Calculate the linear combination of a matrix
#' 
#' @param x numeric matrix
#' @param coeffs numeric vector of coefficients
#' @param version 1 or 2 (default: 1). Allows for two different versions of
#'   calculation both of which should return the same!
#' 
#' @examples 
#' (x <- randomMatrix(c(4, 2)))
#' (coeffs <- rnorm(ncol(x)))
#'   
#' # Calculate the linear combination manually
#' LC1 <- x[, 1] * coeffs[1] + x[, 2] * coeffs[2]
#' 
#' # Caluclate with linearCombination()
#' LC2 <- linearCombination(x, coeffs)
#' 
#' # The result shoulc be the same!
#' all.equal(LC1, LC2) # TRUE
#' 
linearCombination <- function(x, coeffs, version = 1)
{
  if (! is.matrix(x)) {
    
    x <- as.matrix(x)
  }
  
  if (! is.numeric(x)) {
    
    stop("x is not numeric (after coercion to matrix) but ", mode(x))
  }
  
  if (! is.numeric(coeffs)) {
    
    stop("coeffs is not numeric but ", mode(coeffs))
  }
  
  if (ncol(x) != length(coeffs)) {
    
    stop(sprintf(
      paste("The number of columns of x (%d) is not equal to the length",
            "of coeffs (%d)"),
      ncol(x), length(coeffs)
    ))
  }
  
  if (version == 1) {
    
    colSums(t(x) * coeffs)
    
  } else {
    
    apply(x, 1, function(valuesInRow) sum(valuesInRow * coeffs))
  }
}

# randomMatrix -----------------------------------------------------------------

#' Create a Matrix with Random Integer Values
#' 
#' Create a matrix of given dimension and fill it with random integer values
#'
#' @param dim integer vector of length two containing the number of rows and
#'   columns, respectively, that the output matrix shall contain
#' @param values set of values to be used within the matrix
#'  
#' @examples 
#' 
#' # By default, the matrix has a random number of rows between 1 and 10 and
#' # a random number of columns between 1 and 10 and random values of 1:100
#' randomMatrix()
#' 
#' # You may specify the dimensions (here: 5 rows, 3 columns)...
#' randomMatrix(dim = c(5, 3))
#' 
#' # ... and the set of values to be used within the matrix
#' randomMatrix(dim = c(5, 3), values = c(0, 0.5, 1, NA))
#'
randomMatrix <- function(
  dim = c(sample(10, 1), sample(10, 1)), values = seq_len(100)
)
{
  matrix(sample(values, size = dim[1] * dim[2], replace = TRUE), ncol = dim[2])
}

# createMatrix -----------------------------------------------------------------

#' Matrix with Row and Column Names
#' 
#' Create a matrix by giving row and column names and with all elements being 
#' set to a default value
#' 
#' @param rowNames character vector of row names to be given to the matrix
#' @param colNames character vector of column names to be given to the matrix
#' @param value value to be given to each matrix element
#' @param name.row name to be given to the row dimension (default: "")
#' @param name.col name to be given to the column dimension (default: "")
#'   
#' @return matrix with \code{rowNames} as row names and \code{colNames} as
#'   column names, filled with \emph{value} at each position
#'   
#' @examples 
#' ## Initialise a matrix with rows A to E and columns x to z of value -1
#' createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)
#' 
#' ## By default the column names are assumed to be equal to the row names
#' createMatrix(c("A", "B", "C"))
#' 
#' ## Initialise a square matrix with NA
#' createMatrix(c("A", "B", "C"), value = NA)
#' 
#' ## Give a name to the row dimension
#' createMatrix(c("A", "B", "C"), name.row = "Letters")
#' 
createMatrix <- function(
  rowNames, colNames = rowNames, value = 0, name.row = "", name.col = ""
)
{
  stopifnot(is.character(rowNames))
  
  stopifnot(is.character(colNames))
  
  nrows <- length(rowNames)
  
  dimnames <- list(rowNames, colNames)
  
  if (! is.null(name.row)) {
    
    names(dimnames)[1] <- name.row
  }
  
  if (! is.null(name.col)) {
    
    names(dimnames)[2] <- name.col
  }
  
  matrix(
    data = rep(value, times = nrows * length(colNames)), 
    nrow = nrows,
    dimnames = dimnames
  )
}

# setMatrixColumns -------------------------------------------------------------

#' Set Matrix Columns to Values
#' 
#' Set matrix columns of given names to fix values
#' 
#' @param m matrix
#' @param columnValuePairs list of elements each of which defines an assignment
#'   in the form \code{<column-name> = <value>}
#' @param warn if \code{TRUE}, warnings are given if columns named in
#'   \code{columnValuePairs} do not exist in matrix \code{m}
#' 
setMatrixColumns <- function(m, columnValuePairs, warn = TRUE)
{
  stopifnot(is.list(columnValuePairs))
  
  stopifnot(is.matrix(m))
  
  names.list <- names(columnValuePairs)
  
  names.matrix <- colnames(m)
  
  columns.missing <- setdiff(names.list, names.matrix)
  
  if (isTRUE(warn) && length(columns.missing) > 0) {
    
    warning("No such column(s) in matrix 'm': ", stringList(columns.missing))
  }
  
  columns <- intersect(names.list, names.matrix)
  
  for (column in columns) {
    
    m[, column] <- columnValuePairs[[column]]
  }
  
  m
}

# assertRowsAndColumns ---------------------------------------------------------

#' Assert Row and Column Names of a Matrix
#'
#' Make sure that a matrix contains rows and columns of the given names in the
#' given order.
#'
#' @param x A matrix
#' @param row_names character vector of row names
#' @param col_names character vector of column names
#' @param fill_value value to fill a row or column with if a row or column does
#'   not exist in \code{x}
#'
#' @examples
#' m <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(
#'   rows = paste0("row", 1:3), cols = paste0("col", 1:4)
#' ))
#'
#' # Add two rows, reverse order of rows, add one column, remove one column
#' assertRowsAndColumns(
#'   m,
#'   row_names = paste0("row", 4:0),
#'   col_names = paste0("col", 0:2)
#' )
#'
assertRowsAndColumns <- function(
  x, row_names = NULL, col_names = NULL, fill_value = 0
)
{
  kwb.utils::stopIfNotMatrix(x)
  
  if (is.null(row_names) && is.null(col_names)) {
    
    return (x)
  }
  
  row_names <- kwb.utils::defaultIfNULL(row_names, rownames(x))
  
  col_names <- kwb.utils::defaultIfNULL(col_names, colnames(x))
  
  y <- matrix(
    fill_value, nrow = length(row_names), ncol = length(col_names),
    dimnames = structure(list(row_names, col_names), names = names(dimnames(x)))
  )
  
  rows <- intersect(rownames(x), row_names)
  
  cols <- intersect(colnames(x), col_names)
  
  y[rows, cols] <- x[rows, cols]
  
  y
}

# stopIfNotMatrix --------------------------------------------------------------

#' Stop with a Message if Input is not a Matrix
#' 
#' @param x object to be checked with \code{\link{is.matrix}}
#' 
stopIfNotMatrix <- function(x)
{
  if (! is.matrix(x)) {
    
    stop("x ist not a matrix but:\n", utils::capture.output(utils::str(x)))
  }
}

# diffrows ---------------------------------------------------------------------

#' Differences between Matrix Rows
#' 
#' @param x matrix
#' 
#' @return matrix with one row less than in input matrix \code{x} and each row 
#'   \code{i} representing the difference \code{x[i+1, ]-x[i, ]} between rows
#'   \code{i+1} and \code{i} in \code{x}
#'   
#' @examples 
#' x <- matrix(1:12, nrow = 3)
#' 
#' d <- diffrows(x)
#' 
#' x[2, ] - x[1, ] == d[1, ]
#' x[3, ] - x[2, ] == d[2, ]
#' 
diffrows <- function(x)
{
  kwb.utils::stopIfNotMatrix(x)
  
  do.call(rbind, lapply(seq_len(nrow(x) - 1), function(i) x[i + 1, ] - x[i, ]))
}

# asColumnList -----------------------------------------------------------------

#' Matrix to List of Matrix Columns
#' 
#' @param x matrix
#' 
#' @return list with as many elements as there are columns in \code{x} and each
#'   element representing one column
#'   
#' @examples
#' x <- matrix(1:12, nrow = 3)
#' 
#' column_list <- asColumnList(x)
#' 
#' for (i in 1:ncol(x)) print(identical(column_list[[i]], x[, i]))
#' 
asColumnList <- function(x)
{
  kwb.utils::stopIfNotMatrix(x)
  
  structure(lapply(seq_len(ncol(x)), function(i) x[, i]), names = colnames(x))
}

# asRowList --------------------------------------------------------------------

#' Matrix to List of Matrix Rows
#' 
#' @param x matrix
#' 
#' @return list with as many elements as there are rows in \code{x} and each
#'   element representing one row
#'   
#' @examples
#' x <- matrix(1:12, nrow = 3)
#' 
#' row_list <- asRowList(x)
#' 
#' for (i in 1:nrow(x)) print(identical(row_list[[i]], x[i, ]))
#' 
asRowList <- function(x)
{
  kwb.utils::stopIfNotMatrix(x)
  
  structure(lapply(seq_len(nrow(x)), function(i) x[i, ]), names = rownames(x))
}

# linearCombination ------------------------------------------------------------

#' linear combination of a matrix
#' 
#' Calculate the linear combination of a matrix
#' 
#' @param x numeric matrix
#' @param coeffs numeric vector of coefficients
#' @param version 1 or 2 (default: 1). Allows for two different versions of calculation
#'   both of which should return the same!
#' 
#' @examples 
#'   (x <- randomMatrix(c(4, 2)))
#'   (coeffs <- rnorm(ncol(x)))
#'     
#'   # Calculate the linear combination manually
#'   LC1 <- x[, 1] * coeffs[1] + x[, 2] * coeffs[2]
#'   
#'   # Caluclate with linearCombination()
#'   LC2 <- linearCombination(x, coeffs)
#'   
#'   # The result shoulc be the same!
#'   all.equal(LC1, LC2) # TRUE
#'   
#' 
linearCombination <- structure(
  function # linear combination of a matrix
  ### Calculate the linear combination of a matrix
  (
    x, 
    ### numeric matrix
    coeffs, 
    ### numeric vector of coefficients
    version = 1
    ### 1 or 2 (default: 1). Allows for two different versions of calculation
    ### both of which should return the same!
  )
  {
    if (! is.matrix(x)) {
      x <- as.matrix(x)
    }
    
    if (! is.numeric(x)) {
      stop("x is not numeric (after coercion to matrix) but", mode(x))
    }
    
    if (! is.numeric(coeffs)) {
      stop("coeffs is not numeric but", mode(coeffs))
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
    }
    else {
      apply(x, 1, function(valuesInRow) sum(valuesInRow * coeffs))
    }
  }, ex = function() {
    (x <- randomMatrix(c(4, 2)))
    (coeffs <- rnorm(ncol(x)))
      
    # Calculate the linear combination manually
    LC1 <- x[, 1] * coeffs[1] + x[, 2] * coeffs[2]

    # Caluclate with linearCombination()
    LC2 <- linearCombination(x, coeffs)
    
    # The result shoulc be the same!
    all.equal(LC1, LC2) # TRUE
  })

# randomMatrix -----------------------------------------------------------------

#' Create a matrix with random integer values
#' 
#' Create a matrix of given dimension and fill it with random integer values
#'
#' @param dim integer vector of length two containing the number of rows and
#'   columns, respectively, that the output matrix shall contain
#' @param values set of values to be used within the matrix
#'  
#' @examples 
#'   
#'   # By default, the matrix has a random number of rows between 1 and 10 and
#'   # a random number of columns between 1 and 10 and random values of 1:100
#'   randomMatrix()
#'   
#'   # You may specify the dimensions (here: 5 rows, 3 columns)...
#'   randomMatrix(dim = c(5, 3))
#'   
#'   # ... and the set of values to be used within the matrix
#'   randomMatrix(dim = c(5, 3), values = c(0, 0.5, 1, NA))
#'
randomMatrix <- function
(
  dim = c(sample(10, 1), sample(10, 1)),
  values = seq_len(100)
)
{
  matrix(sample(values, size = dim[1] * dim[2], replace = TRUE), ncol = dim[2])
}

# createMatrix -----------------------------------------------------------------

#' matrix with row and column names
#' 
#' Create a matrix by giving row and column names and with all elements being
#'   set to a default value
#' 
#' @param rowNames character vector of row names to be given to the matrix
#' @param colNames character vector of column names to be given to the matrix
#' @param value value to be given to each matrix element
#' @param name.row name to be given to the row dimension (default: "")
#' @param name.col name to be given to the column dimension (default: "")
#' 
#' @return matrix with \code{rowNames} as row names and \code{colNames} as column
#'   names, filled with \emph{value} at each position
#' 
#' @examples 
#'   ## Initialise a matrix with rows A to E and columns x to z of value -1
#'   createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)
#'   
#'   ## By default the column names are assumed to be equal to the row names
#'   createMatrix(c("A", "B", "C"))
#'   
#'   ## Initialise a square matrix with NA
#'   createMatrix(c("A", "B", "C"), value = NA)
#'   
#'   ## Give a name to the row dimension
#'   createMatrix(c("A", "B", "C"), name.row = "Letters")
#'   
#' 
createMatrix <- structure(
  function # matrix with row and column names
  ### Create a matrix by giving row and column names and with all elements being
  ### set to a default value
  (
    rowNames,
    ### character vector of row names to be given to the matrix
    colNames = rowNames,
    ### character vector of column names to be given to the matrix
    value = 0,
    ### value to be given to each matrix element
    name.row = "",
    ### name to be given to the row dimension (default: "")
    name.col = ""
    ### name to be given to the column dimension (default: "")
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
    
    ### matrix with \code{rowNames} as row names and \code{colNames} as column
    ### names, filled with \emph{value} at each position
  }, ex = function() {
    ## Initialise a matrix with rows A to E and columns x to z of value -1
    createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)
    
    ## By default the column names are assumed to be equal to the row names
    createMatrix(c("A", "B", "C"))
    
    ## Initialise a square matrix with NA
    createMatrix(c("A", "B", "C"), value = NA)
    
    ## Give a name to the row dimension
    createMatrix(c("A", "B", "C"), name.row = "Letters")
  })

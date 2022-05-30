# extractRowRanges -------------------------------------------------------------

#' Extract Row Ranges by Pattern
#' 
#' @param x data frame or matrix or vector of character (representing e.g. the 
#'   rows read from a text file)
#' @param pattern pattern to be searched for, either in \code{[, column]} (if
#'   \code{x} is a data frame or a matrix) or in \code{x} (if \code{x} is a 
#'   vector of character)
#' @param column name of column in which to search for \emph{pattern} if 
#'   \code{x} is a data frame or a matrix
#' @param starts optional. Vector of indices representing the starts of the row
#'   ranges to be extracted. This argument overrides \code{pattern}. Instead of
#'   using the pattern to find the start indices, the indices given here are 
#'   used.
#' @param startOffset row offset to be added to row number in which the pattern
#'   matches
#' @param stopOffset row offset to be subtracted from row number in which the
#'   pattern matches
#' @param nameByMatch logical. if \code{TRUE}, the elements in the result list
#'   are named by the matching values. Defaults to \code{FALSE}.
#' @param nameColumnsByMatch if \code{TRUE} (default) the columns of the result
#'   data frames or matrices are named (if \code{x} is a data frame or a matrix)
#' @param renumber if \code{TRUE} (default) and \code{x} is a data frame or a 
#'   matrix the row names of the result data frames or matrices are reset to 
#'   \code{NULL}, i.e. their rows are renumbered
#' @return list of data frames or list of matrices or list of vectors of
#'   character. Each list element represents one section of the input, found
#'   between two consecutive matches of \code{pattern}.
#' @export
#' @examples 
#' textLines <- c(
#'   "Date,Value",
#'   "1.1.,1",
#'   "2.1.,2",
#'   ",",
#'   "Date,Value",
#'   "3.1.,3",
#'   "4.1.,4",
#'   ",",
#'   "Date,Value",
#'   "5.1.,5",
#'   "6.1.,6"
#' )
#' 
#' # Convert textLines to data frame. The function should be able to handle both.
#' (dataFrame <- read.table(text = textLines, sep = ",", stringsAsFactors = FALSE))
#' 
#' # stopOffset = 2L: skip empty line at the bottom of each sub-table
#' extractRowRanges(
#'   textLines,
#'   pattern = "Date", 
#'   stopOffset = 2L,
#' )
#' 
#' extractRowRanges(
#'   dataFrame,
#'   pattern = "Date",
#'   column = "V1",
#'   stopOffset = 2L
#' )
#' 
#' # Extract sections after a header line
#' # startOffset = 2L: skip empty line after header "topic: ..."
#' textLines <- c(
#'   "topic: A",
#'   "",
#'   " a.1",
#'   " a.2",
#'   " a.3",
#'   "topic: B",
#'   "",
#'   " b.1",
#'   "topic: C",
#'   "",
#'   " c.1",
#'   " c.2"
#' )
#' 
#' extractRowRanges(
#'   textLines,
#'   pattern = "topic", 
#'   startOffset = 2L,
#'   nameByMatch = TRUE
#' )
extractRowRanges <- function(
  x, pattern, column = NULL, starts = NULL, startOffset = 1L, stopOffset = 1L,
  nameByMatch = FALSE, nameColumnsByMatch = TRUE, renumber = TRUE
)
{
  isTwoDimensional <- length(dim(x)) == 2L
  
  values <- if (isTwoDimensional) {
    stopifnot(!is.null(column), is.character(column), length(column) == 1L)
    if (is.data.frame(x)) {
      selectColumns(x, column)
    } else {
      stopIfNotMatrix(x)
      stopifnot(column %in% colnames(x))
      x[, column, drop = FALSE]
    }
  } else {
    as.character(x)
  }
  
  # Find the rows of the data frame or the indices in the vector of strings 
  # where the values in column <column> or the elements in vector 
  # <x> match the pattern
  starts <- kwb.utils::defaultIfNULL(starts, grep(pattern, values))

  # Create index ranges from one start position to (one before) the next  
  ranges <- startsToRanges(
    starts = starts, 
    lastStop = length(values), 
    startOffset = startOffset,
    stopOffset = stopOffset
  )

  # Loop through the ranges and create row subsets or subsets of vector elements
  result <- lapply(seq_len(nrow(ranges)), function(i) {
    
    from <- ranges$from[i]
    to <- ranges$to[i]
    
    result <- if (from <= to) {
      if (isTwoDimensional) {
        x[from:to, , drop = FALSE]
      } else {
        x[from:to]
      }
    } # else NULL implicitly
    
    if (is.null(result)) {
      return(NULL)
    }
    
    if (isTwoDimensional) {
      if (nameColumnsByMatch) {
        colnames(result) <- as.character(x[starts[i], ])
      }
      if (renumber) {
        row.names(result) <- NULL  
      }
    }

    result
  })
  
  if (nameByMatch) {
    names(result) <- makeUnique(warn = FALSE, sapply(
      values[starts], kwb.utils::substSpecialChars
    ))
  }
  
  result
}

# startsToRanges ---------------------------------------------------------------

#' Row Numbers of Start Rows to From/To Row Ranges
#' 
#' A vector of row numbers is transformed to a data frame describing row ranges 
#' by numbers of  first and last rows
#' 
#' @param starts integer vector of start indices
#' @param lastStop integer value of the last stop index
#' @param startOffset integer offset applied to the starts
#' @param stopOffset integer offsets applied to the ends
#' @return data frame with columns \code{from} and \code{to}
#' @export
#' @examples 
#' starts <- c(1, 10, 20, 35)
#' 
#' ok <- identical(
#'   startsToRanges(starts, lastStop = 50), 
#'   data.frame(
#'     from = c(2, 11, 21, 36),
#'     to = c(9, 19, 34, 50)
#'   )
#' )
#'   
#' ok <- ok && identical(
#'   startsToRanges(starts, lastStop = 55, startOffset = 2, stopOffset = 2), 
#'   data.frame(
#'     from = c(3, 12, 22, 37),
#'     to = c(8, 18, 33, 55)
#'   )
#' )
#'   
#' ok
#'   
startsToRanges <- function(starts, lastStop, startOffset = 1, stopOffset = 1)
{
  data.frame(
    from = starts + startOffset,
    to = startsToEnds(starts, lastStop, stopOffset)
  )
}

# startsToEnds -----------------------------------------------------------------

#' Helper Function: Start Indices to End Indices
#' 
#' helper function to convert start indices to end indices
#' 
#' @param starts vector of integer
#' @param lastStop number to be returned as last element of the result vector
#' @param stopOffset number to be subtracted from (all but the first elements
#'   in) \emph{starts} in order to find the ends
#' @return vector of integer
#' @export
#' @examples 
#' starts <- c(1, 10, 20, 35)
#' 
#' ok <- identical(
#'   startsToEnds(starts, lastStop = 50), 
#'   c(9, 19, 34, 50)
#' )
#'   
#' ok <- ok && identical(
#'   startsToEnds(starts, lastStop = 50, stopOffset = 2), 
#'   c(8, 18, 33, 50)
#' )
#' 
#' ok
#' 
startsToEnds <- function(starts, lastStop, stopOffset = 1)
{
  c(starts[-1] - stopOffset, lastStop)
}

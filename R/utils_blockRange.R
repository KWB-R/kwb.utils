# M A I N ----------------------------------------------------------------------
if (FALSE) 
{
  .test_startsToEnds()
  .test_startsToRanges()
  .test_extractRowRanges()
}

# .test_extractRowRanges --------------------------------------------------------
.test_extractRowRanges <- function()
{
  dataFrame <- as.data.frame(
    matrix(
      c("Date", "Value",
        "1.1.", "1",
        "2.1.", "2",
        "", "",
        "Date", "Value",
        "3.1.", "3",
        "4.1.", "4"), 
      ncol = 2, 
      byrow = TRUE
    ),
    stringsAsFactors = FALSE
  )
  
  y <- extractRowRanges(
    dataFrame, columnName = "V1", pattern = "Date", stopOffset = 2
  )
  
  expected <- list(
    data.frame(
      Date = c("1.1.", "2.1."),
      Value = c("1", "2"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Date = c("3.1.", "4.1."),
      Value = c("3", "4"),
      stringsAsFactors = FALSE
    )
  )
  
  identical(y, expected)
}

# .test_startsToRanges ----------------------------------------------------------
.test_startsToRanges <- function()
{
  starts <- c(1, 10, 20, 35)
  
  ok <- identical(startsToRanges(starts, lastStop = 50), 
                  data.frame(
                    from = c(2, 11, 21, 36),
                    to = c(9, 19, 34, 50)
                  ))
  
  ok <- ok && identical(
    startsToRanges(starts, lastStop = 55, startOffset = 2, stopOffset = 2), 
    data.frame(
      from = c(3, 12, 22, 37),
      to = c(8, 18, 33, 55)
    ))
  
  ok
}

# .test_startsToEnds -----------------------------------------------------------
.test_startsToEnds <- function()
{
  starts <- c(1, 10, 20, 35)
  
  ok <- identical(startsToEnds(starts, lastStop = 50), 
                  c(9, 19, 34, 50)) 
  
  ok <- ok && identical(startsToEnds(starts, lastStop = 50, stopOffset = 2), 
                        c(8, 18, 33, 50))
  ok
}

# extractRowRanges -------------------------------------------------------------
extractRowRanges <- structure(
  function # extract row ranges by pattern
  ### extract row ranges by pattern
  (
    dataFrame, 
    ### data frame
    columnName, 
    ### name of column in which to search for \emph{pattern}
    pattern, 
    ### pattern to be searched for in \code{dataFrame[[columnNa,e]]}
    startOffset = 1, 
    ### row offset to be added to row number in which the pattern matches
    stopOffset = 1,
    ### row offset to be subtracted from row number in which the pattern matches
    nameByMatch = FALSE,
    ### logical. if TRUE, the elements in the result list are named by the 
    ### matching values in \code{dataFrame[[columnName]]}. Defaults to FALSE.
    nameColumnsByMatch = TRUE,
    renumber = TRUE
  )
{
    checkForMissingColumns(dataFrame, columnName)
    
    starts <- grep(pattern, dataFrame[[columnName]])
    
    ranges <- startsToRanges(
      starts = starts, 
      lastStop = nrow(dataFrame), 
      startOffset = startOffset,
      stopOffset = stopOffset
    )
    
    result <- lapply(seq_len(nrow(ranges)), function(i) {
      result <- dataFrame[ranges$from[i]:ranges$to[i], ]
      if (nameColumnsByMatch) {
        names(result) <- as.character(dataFrame[starts[i], ])
      }
      if (renumber) {
        row.names(result) <- NULL  
      }      
      result
    })
    
    if (nameByMatch) {
      names(result) <- dataFrame[starts, columnName]
    }
    
    result
    ### list of data frames containing the rows of \emph{dataFrame} between
    ### rows matching \emph{pattern} in \code{dataFrame[[columnName]]}.
  }, 
ex = .test_extractRowRanges
)

# startsToRanges ---------------------------------------------------------------
startsToRanges <- structure(
  function # row numbers of start rows to from/to row ranges
  ### a vector of row numbers is transformed to a data frame describing 
  ### row ranges by numbers of  first and last rows
  (
    starts, 
    lastStop, 
    startOffset = 1, 
    stopOffset = 1
  )
  {
    data.frame(
      from = starts + startOffset,
      to = startsToEnds(starts, lastStop, stopOffset)
    )
    
    # data frame with columns \emph{from} and \emph{to}
  }, 
  ex = .test_startsToRanges
)

# startsToEnds -----------------------------------------------------------------
startsToEnds <- structure(
  function # helper function: start indices to end indices
  ### helper function to convert start indices to end indices
  (
    starts, 
    ### vector of integer
    lastStop, 
    ### number to be returned as last element of the result vector
    stopOffset = 1
    ### number to be subtracted from (all but the first elements in) \emph{starts}
    ### in order to find the ends
  )
  {
    c(starts[-1] - stopOffset, lastStop)
    ### vector of integer
  }, 
  ex = .test_startsToEnds  
)

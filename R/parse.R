# guessSeparator ---------------------------------------------------------------

#' Guess Column Separator Used in File
#' 
#' @param csvFile full path to text file containing 'comma separated values'
#' @param n number of first lines in the file to be looked at
#' @param separators vector of possible column separator characters the file is
#'   to be checked for
#' 
guessSeparator <- function(csvFile, n = 10, separators = c(";", ",", "\t"))
{ 
  if (length(csvFile) > 1) {
    
    structure(sapply(csvFile, guessSeparator, n = n), names = NULL)
    
  } else {
    
    stopifnot(file.exists(csvFile))    
    
    textlines <- readLines(csvFile, n)
    
    # Try method 1
    candidates <- .guessSeparator.1(textlines, separators)
    
    if (length(candidates) == 1) {
      
      return(candidates)
    }
    
    # Try method 2
    candidates <- .guessSeparator.2(textlines, separators)
    
    if (length(candidates) == 1) {
      
      return(candidates)
    }

    # Try other methods...
    if (length(candidates) == 0) {
      
      messageText <- paste(
        "None of the separators results in the same number",
        "of columns for each row"
      )
      
    } else if (length(candidates) > 1) {
      
      messageText <- paste(
        "I cannot decide between these separators:", 
        paste0("'", candidates, "'", collapse = ", ")
      )
    }
    
    warning(sprintf(
      paste("When guessing the separator of '%s': %s.",
            "Returning the first separator '%s'!"),
      csvFile, messageText, separators[1]
    ))
    
    separators[1]
  }
}

# .guessSeparator.1 ------------------------------------------------------------

#' Guess Column Separator (Version 1)
#' 
#' 
.guessSeparator.1 <- function(textlines, separators, comment.char = "#")
{
  #
  # Method 1: 
  # - 1) calculate the number of columns in each line
  # - 2) For which separator is the number of columns equal in all rows and 
  #      greater than 1?
  #
  
  numberOfColumns <- lapply(separators, function(sep) {
    
    unlist(lapply(textlines, function(line) {
      
      if (grepl("^\\s*$", line) || 
            comment.char != "" && stringStartsWith(hsTrim(line), comment.char)) {
        0
        
      } else {
        
        ncol(csvTextToDataFrame(line, sep = sep, comment.char = comment.char))
      }
    }))
  })
  
  selected <- sapply(numberOfColumns, function(x) {
    
    x[1] > 1 && allAreEqual(x)
  })
  
  separators[selected]  
}

# .guessSeparator.2 ------------------------------------------------------------

#' Guess Column Separator (Version 2)
#' 
.guessSeparator.2 <- function(textlines, separators)
{
  # Which of the separators occurs most?
  characters <- strsplit(paste(textlines, collapse = ""), "")[[1]]
  
  count <- sapply(separators, function(sep) {
    
    sum(characters == sep)
  })
  
  names(which.max(count))
}

# getKeywordPositions ----------------------------------------------------------

#' Localise Keywords in Data Frame
#' 
#' @param dataFrame data frame or matrix in which to search for given keywords
#' @param keywords (list of) keywords to be looked for in \emph{data frame}
#' @param asDataFrame if TRUE (default), a data frame is returned, otherwise a
#'   matrix
#'   
#' @return data frame (if \emph{asDataFrame} = TRUE) or matrix with one column
#'   per keyword that was given in \emph{keywords}. The first row contains the
#'   row numbers and the second row contains the column numbers, respectively,
#'   of the fields in \emph{dataFrame} in which the corresponding keywords were 
#'   found.
#' 
getKeywordPositions <- function(dataFrame, keywords, asDataFrame = TRUE)
{
  textValues <- as.matrix(dataFrame)
  
  keywordPositions <- sapply(keywords, function(x) {
    
    which(textValues == x, arr.ind = TRUE)
  })
  
  # the result must be a matrix, otherwise a keyword was not found exacly once
  if (! is.matrix(keywordPositions)) {
    
    stop(
      "\n*** I could not find all of these keywords that I was looking for:\n",
      paste("'", as.character(keywords), "'", sep = "", collapse = ", "))
  }
  
  # data frame allows for the $ operator later
  if (asDataFrame) {
    
    as.data.frame(keywordPositions)  
    
  } else {
    
    keywordPositions
  }
}

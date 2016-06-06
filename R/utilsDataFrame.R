# .test_compareDataFrames ------------------------------------------------------
.test_compareDataFrames <- function()
{
  x <- data.frame(a = 1:2, b = 2:3)
  y <- x
  
  test1 <- all(unlist(compareDataFrames(x, y)))
  
  z <- compareDataFrames(x, y[, c("b", "a")])
  expectedFalse <- c("identical", "identicalExceptAttributes", "sameColumnNames")  
  test2 <- all(names(which(!unlist(z))) == expectedFalse)
  
  test1 && test2
}

# compareDataFrames ------------------------------------------------------------
compareDataFrames <- structure(
  function # compare two data frames by columns
  ### compare two data frames by columns
  (
    x, 
    ### first data frame
    y
    ### second data frame
  )
  {
    stopifnot(is.data.frame(x))
    stopifnot(is.data.frame(y))
    
    typeToName <- c(Column = "names", Row = "row.names")
    
    names.x <- names(x)
    names.y <- names(y)
    
    row.names.x <- row.names(x)
    row.names.y <- row.names(y)
    
    result <- list()
    
    result$identical <- identical(x, y)
    
    result$identicalExceptAttributes <- identical(
      kwb.utils::removeAttributes(x), 
      kwb.utils::removeAttributes(y)
    )
    
    result$equalNumberOfRows <- (nrow(x) == nrow(y))
    
    result$equalNumberOfColumns <- (ncol(x) == ncol(y))
    
    for (type in names(typeToName)) {
      
      if (result[[sprintf("equalNumberOf%ss", type)]]) {
        
        objects.x <- get(paste0(typeToName[type], ".x"))
        objects.y <- get(paste0(typeToName[type], ".y"))
        
        check <- all(objects.x == objects.y)
        result[[sprintf("same%sNames", type)]] <- check
        
        check <- (length(setdiff(objects.x, objects.y)) == 0)
        result[[sprintf("same%sNamesOrdered", type)]] <- check
      }
    }
    
    check <- sapply(unique(c(names.x, names.y)), function(column) {
      c(
        identical = identical(x[[column]], y[[column]]),
        identicalExceptAttributes = identical(               
          kwb.utils::removeAttributes(x[[column]]),
          kwb.utils::removeAttributes(y[[column]])
        ),
        equalValues = all(x[[column]] == y[[column]])
      )
    })
    
    result$byColumn <- check
    
    result
    ### list of logical
  }, 
  ex = .test_compareDataFrames
)

#
# Functions on data frames: row-related ----------------------------------------
#

# atLeastOneRowIn --------------------------------------------------------------
atLeastOneRowIn <- function # at least one row in data frame
### returns TRUE if data frame has at least one row, else FALSE
(
  dframe
  ### data frame
) 
{
  nrow(dframe) > 0
}

# .test_rbindAll ---------------------------------------------------------------
.test_rbindAll <- function()
{
  L <- list(
    A = data.frame(x = 1:2, y = 2:3),
    B = data.frame(x = 1:3, y = 2:4)
  )
  
  L.unnamed <- L
  names(L.unnamed) <- NULL
  
  y1 <- rbindAll(L)
  y2 <- rbindAll(L, nameColumn = "group")
  y3 <- rbindAll(L.unnamed, nameColumn = "group", namesAsFactor = FALSE)
  y4 <- rbindAll(L.unnamed, nameColumn = "group")
  
  expected1 <- data.frame(
    x = c(L$A$x, L$B$x), 
    y = c(L$A$y, L$B$y)
  )
  
  expected2 <- cbind(
    expected1, 
    group = as.factor(c(rep("A", nrow(L$A)), rep("B", nrow(L$B)))),
    stringsAsFactors = FALSE
  )
  
  expected3 <- cbind(
    expected1, 
    group = c(rep(1L, nrow(L$A)), rep(2L, nrow(L$B)))
  )
  
  expected4 <- expected3
  expected4$group <- as.factor(expected4$group)
  
  identical(y1, expected1) && 
    identical(y2, expected2) && 
    identical(y3, expected3) && 
    identical(y4, expected4)
}

# rbindAll ---------------------------------------------------------------------
rbindAll <- structure(
  function # rbind all data frames given in a list
  ### rbind all data frames given in a list
  (
    x,
    ### list of data frames to be passed to \code{rbind}
    nameColumn = "",
    ### optional. If given, an additional column of that name is added to the
    ### resulting data frame containing the name (or number if \emph{args} is
    ### an unnamed list) of the element in \emph{x} that the corresponding rows 
    ### belong to
    remove.row.names = TRUE,
    ### if TRUE (default) row names are reset in the output data frame
    namesAsFactor = TRUE
    ### if TRUE (default) and \emph{nameColumn} is given the values in 
    ### column \emph{nameColumn} are converted to a factor
  )
  {
    result <- do.call(rbind, x)
    
    if (nameColumn != "") {
      
      xnames <- names(x)
      
      if (is.null(xnames)) {
        xnames <- seq_len(length(x))
      }
      
      nameValues <- rep(xnames, times = sapply(x, nrow))
      
      if (namesAsFactor) {
        nameValues <- as.factor(nameValues)
      }
      
      result[[nameColumn]] <- nameValues
    }
    
    if (remove.row.names) {
      row.names(result) <- NULL
    }
    
    result
  }, ex = .test_rbindAll
)

# safeRowBindOfListElements ----------------------------------------------------
safeRowBindOfListElements <- structure(
  function # row-bind data frames in a list of lists
  ### row-bind data frames in a list of lists
  (
    x, 
    ### list of lists each of which contains a data frame in element
    ### \emph{elementName}
    elementName
    ### name of list element in each sublist of \emph{x} which contains a 
    ### data frame
  )
  {
    x.list <- lapply(x, "[[", elementName)
    
    result <- NULL
    
    for (dataFrame in x.list) {
      result <- safeRowBind(result, dataFrame)
    }
    
    result
    ### data frame resulting from "row-binding" data frames. 
  }, ex = function() {
    x <- list(
      list(
        number = 1, 
        data = data.frame(x = 1:2, y = 2:3)
      ),
      list(
        number = 2,
        data = data.frame(x = 11:12, y = 12:13)
      )
    )
    
    safeRowBindOfListElements(x, "data")
    
    ## also working if the column names of the data frames in the "data" elements
    ## differ.
    x[[1]]$data$z = 13:14
    safeRowBindOfListElements(x, "data")  
  })

# safeRowBind ------------------------------------------------------------------
safeRowBind <- function # "safe" rbind
### rbind two data frames even if column names differ
(
  dataFrame1, 
  dataFrame2
)
{
  stopifnot((is.null(dataFrame1) || is.data.frame(dataFrame1)) && 
              (is.null(dataFrame2) || is.data.frame(dataFrame2)))
  
  if (is.null(dataFrame1)) {
    return(dataFrame2)
  }
  
  if (is.null(dataFrame2)) {
    return(dataFrame1)
  }
  
  allColumnNames <- unique(c(names(dataFrame1), 
                             names(dataFrame2)))
  
  dataFrame1 <- hsAddMissingCols(dataFrame1, allColumnNames)
  dataFrame2 <- hsAddMissingCols(dataFrame2, allColumnNames)
  
  rbind(dataFrame1, dataFrame2)  
}

# addRowWithName ---------------------------------------------------------------
addRowWithName <- function # addRowWithName
### add row to data frame and give a row name at the same time
(
  x, 
  ### data frame to which row is to be appended
  y, 
  ### data frame containing the row to be appended (exacly one row expected)
  row.name
  ### name of row to be given in result data frame
)
{
  stopifnot(nrow(y) == 1)
  
  x <- rbind(x, y)
  row.names(x)[nrow(x)] <- row.name
  
  return(x)
  ### \emph{x} with row of \emph{y} (named \emph{row.name}) appended to it
}

#
# Functions on data frames: column-related -------------------------------------
#

# mergeAll ---------------------------------------------------------------------
mergeAll <- structure(
  function # merge multiple data frames 
  ### merge multiple data frames, given in a list
  (
    dataFrames, 
    ### list of data frames. If the list elements are named, the element names
    ### are used as suffixes in the column names, otherwise suffixes ".1", ".2",
    ### etc are used
    by, 
    ### vector of column names to be merged by, passed on to \code{merge}
    ...
    ### additional arguments passed to \code{merge}
  )
  {
    stopifnot(!isNullOrEmpty(dataFrames))
    
    # Initialise the result data frame with the first given data frame
    result <- dataFrames[[1]]
    
    # Get the data frame names or take their position numbers as their names
    dataFrameNames <- names(dataFrames)
    
    if (is.null(dataFrameNames)) {
      dataFrameNames <- as.character(seq_len(length(dataFrames)))
    }
    
    # Generate column name suffixes
    suffixes <- paste0(".", dataFrameNames)
    
    # Append the first suffix to the non-key columns
    names(result) <- appendSuffix(
      values = names(result), 
      suffix = suffixes[1], 
      valuesToOmit = by
    )
    
    # Loop through list indices 2 .. length(dataFrames)
    indices <- seq(from = 2, length.out = length(dataFrames) - 1)
    
    for (i in indices) {
      
      # Append the respective suffix to the non-key columns
      names(dataFrames[[i]]) <- appendSuffix(
        values = names(dataFrames[[i]]),
        suffix = suffixes[i], 
        valuesToOmit = by
      )
      
      # Merge the current data frame to the result data frame
      result <- merge(result, dataFrames[[i]], by = by, ...)    
    }
    
    result
    ### data frame being the result of merging all the data frames given in 
    ### \emph{dataFrames} by  consecutively calling \code{merge}
  }, ex = function() {
    peter <- data.frame(fruit = c("apple", "pear", "banana"), kg = 1:3)
    paul <- data.frame(fruit = c("banana", "apple", "lemon"), kg = c(10, 20, 30))
    mary <- data.frame(fruit = c("lemon", "organger", "apple"), kg = c(22, 33, 44))
    
    # By default only categories that are in all data frames are returned  
    mergeAll(list(peter = peter, paul = paul, mary = mary), by = "fruit")
    
    # Use the arguments supported by merge to change that behaviour
    mergeAll(list(peter = peter, paul = paul, mary = mary), by = "fruit", all = TRUE)
  })

# safeColumnBind ---------------------------------------------------------------
safeColumnBind <- structure(
  function # cbind(x1, x2) or x2 if x1 is NULL
  ### "Safe" version of cbind. If \emph{x1} is NULL \emph{x2} is returned otherwise
  ### \code{cbind(x1, x2)}
  (
    x1, x2
  )
  {
    if (is.null(x1)) {
      x2
    } 
    else {
      cbind(x1, x2)
    }
    ### result of \code{cbind(x1, x2)} or \emph{x2} if \emph{x1}
    ### is null. 
  }, ex = function(){
    x1 <- NULL
    
    for (i in 1:3) {
      
      x2 <- data.frame(a = 1:3, b = rnorm(3))
      x1 <- safeColumnBind(x1, x2)
      
      # using cbind would result in an error: 
      # x1 <- cbind(x1, x2)
    }
    
    x1
  })

# posixColumnAtPosition --------------------------------------------------------
posixColumnAtPosition <- function # posixColumnAtPosition
### posixColumnAtPosition
(
  x
  ### data frame containing a date/time column
)
{
  # find a POSIXt-column
  FUN <- function(colname) {
    "POSIXct" %in% class(x[[colname]])
  }
  
  tcol <- which(sapply(names(x), FUN))
  
  if (isNullOrEmpty(tcol)) {
    warning("No POSIXt-column in data frame.")
  }
  
  tcol
}

# firstPosixColumn -------------------------------------------------------------
firstPosixColumn <- function # data/time column of data frame
### data/time column of data frame
(
  x
) 
{
  stopifnot (is.data.frame(x))
  
  x[[posixColumnAtPosition(x)]]  
}

# roundColumns -----------------------------------------------------------------
roundColumns <- function # roundColumns
### roundColumns
(
  dframe, 
  ### data frame containing numeric columns to be rounded
  columnNames = NULL, 
  ### names of (numeric) columns in \emph{dframe} to be rounded.
  digits = NULL
  ### number of digits to be rounded to (vector of length 1 expected) or list of
  ### assignments in the form: \emph{columnName} = \emph{numberOfDigits}. If 
  ### you give a list here, then there is no need to set the argument 
  ### \emph{columnNames}  
) 
{
  # if column names are given we expect that all these columns are rounded to 
  # one and the same number of digits
  if (!is.null(columnNames)) {
    stopifnot(length(digits) == 1)    
  }
  
  if (!is.null(digits)) {
    
    if (is.null(columnNames)) {
      columnNames <- names(digits)
    }
    
    for (columnName in columnNames) {
      
      if (is.list(digits)) {
        numberOfDigits <- digits[[columnName]]
      }
      else {
        numberOfDigits <- digits
      }
      
      dframe[[columnName]] <- round(dframe[[columnName]], digits = numberOfDigits)
    }
  }
  
  return(dframe)
  ### \emph{dframe} with columns given in \emph{columnNames} being rounded to
  ### \emph{digits} digits.
}

# checkForMissingColumns: Check for column existence ---------------------------
checkForMissingColumns <- function # Check for column existence
### Stops if data frame \emph{frm} does not contain all columns of which the
### names are given in \emph{reqCols}.
(
  frm, 
  ### data frame 
  reqCols,
  ### vector of names of which existence in \emph{frm} shall be checked
  do.stop = TRUE
  ### if TRUE, stop() is called else warning() if a column is missing
) 
{
  columnNames <- names(frm)
  
  missing <- reqCols[!(reqCols %in% columnNames)]
  
  if (!isNullOrEmpty(missing)) {
    
    infotext <- paste(
      paste("Columns missing in data frame:", commaCollapsed(missing)),
      paste("Available columns:", commaCollapsed(columnNames)),
      sep="\n")
    
    if (do.stop) {
      stop(infotext)
    }
    else {
      warning(infotext)
    }
  }
  
  return (isNullOrEmpty(missing))
  ### TRUE if all required columns are available, else FALSE
}

# hsAddMissingCols: Add missing columns to data frame --------------------------
hsAddMissingCols <- function # Add missing columns to data frame
### Adds missing columns to the given data frame so that the resulting 
### data frame contains all columns given in the vector \emph{colNames}. 
### Added columns are filled with NA values.
(
  dataFrame,
  ### data frame to which column names are to be appended
  colNames,
  ### vector containing names of columns that shall be contained in 
  ### \emph{dataFrame}  
  fill.value = NA
  ### value to be inserted into newly created columns. Default: NA
)
{
  n <- nrow(dataFrame)
  for (col in colNames) {
    if (! col %in% names(dataFrame)) {
      dataFrame[[col]] <- rep(fill.value, n)
    }
  }
  dataFrame
  ### data frame with columns as listed in \emph{colNames}
}

# hsDelEmptyCols ---------------------------------------------------------------
hsDelEmptyCols <- function # Delete empty columns of data frame
### Returns data frame in which all empty columns (NA in all rows) are removed
(
  dataFrame
  ### data frame of which empty columns (NA in all rows) are to be removed
)
{
  res <- NULL
  for (col in names(dataFrame)) {
    if (! all(is.na(dataFrame[col]))) {
      if (is.null(res)) {
        res <- dataFrame[col]
      }
      else {
        res <- cbind(res, dataFrame[col])        
      }      
    }
  }
  res 
  ### copy of input data frame but with all empty columns removed
}

# removeColumns ----------------------------------------------------------------
removeColumns <- function # remove columns from data frame
### remove columns from a data frame
(
  dframe ,
  ### data frame, 
  columnsToRemove,
  ### vector of column names giving the columns to remove
  drop = FALSE
  ### if FALSE, a data frame is returned in any case, otherwise the result may
  ### be a vector if only one column remains
)
{
  remainingColumns <- setdiff(names(dframe), columnsToRemove)
  hsRestoreAttributes(dframe[, remainingColumns, drop=drop], attributes(dframe))  
  ### \emph{dframe} with columns given in \emph{columnsToRemove} being removed.
  ### User attributes of \emph{dframe} are restored.
}

# hsRenameColumns --------------------------------------------------------------
hsRenameColumns <- function # rename columns in a data frame
### rename columns in a data frame giving tupels of original name
### and substitute name as named elements in list "renames"
(
  dframe ,
  ### data.frame, 
  renames
  ### list with named elements each of which defines a column rename in the form
  ### <old-name> = <new-name>
)
{
  columnNames <- names(dframe)
  for (columnName in names(renames)) {
    columnNames[columnNames == columnName] <- renames[[columnName]]
  }
  names(dframe) <- columnNames
  dframe
  ### \emph{dframe} with columns renamed as specified in \emph{renames}
}

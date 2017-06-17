# fullySorted ------------------------------------------------------------------

#' Sort a Data Frame by all of its Columns
#'
#' @param x data frame
#' @param decreasing passed to \code{\link[base]{order}}
#' @param ... further arguments passed to \code{\link[base]{order}}
#' @param renumber.rows if \code{TRUE} (default) the rows in the returned 
#' data frame are renumbered from 1 to the number of rows in \code{x}
#' @examples
#' fullySorted(head(iris))
#' fullySorted(head(iris), decreasing = TRUE)
#' fullySorted(head(iris[, 5:1]))
#' fullySorted(head(iris[, 5:1]), decreasing = TRUE)
#' 
fullySorted <- function(x, decreasing = FALSE, ..., renumber.rows = TRUE)
{
  stopifnot(is.data.frame(x))
  
  roworder <- do.call(order, c(x, list(decreasing = decreasing, ...)))
  x <- x[roworder, , drop = FALSE]
  
  if (renumber.rows) {
    resetRowNames(x)
  } else {
    x
  }
}

# splitIntoFixSizedBlocks ------------------------------------------------------

#' Split into blocks of same size
#' 
#' Split a data frame or matrix into blocks of the same size (= data frames of 
#' matrices with the same number of rows)
#' 
#' @param data data frame or matrix
#' @param blocksize number of rows in each block into which \code{data} is split
#'   
#' @return list of data frames (if \code{data} is a data frame) or list of
#'   matrices (if \code{data} is a matrix)
#'   
splitIntoFixSizedBlocks <- function(data, blocksize)
{
  stopifnot(length(dim(data)) == 2)

  n <- nrow(data)

  lapply(0:as.integer(n/blocksize), function(i) {
    firstIndex <- i * blocksize + 1
    lastIndex <- (i + 1) * blocksize
    data[firstIndex:min(lastIndex, n), ]
  })

  ### list of data frames (if \code{data} is a data frame) or list of matrices
  ### (if \code{data} is a matrix)
}

# resetRowNames ----------------------------------------------------------------

#' Reset row names to 1:n
#' 
#' Reset the row names of a data frame x to 1:nrow(x) by setting the
#'   \code{row.names} attribute to \code{NULL}.
#' 
#' @param x data frame or matrix
#' 
#' @examples 
#'   persons <- data.frame(id = c(1, 2, 3), name = c("Peter", "Paul", "Mary"))
#'   
#'   persons.ordered <- persons[order(persons$name), ]
#'   
#'   # Original row names
#'   persons.ordered
#'   
#'   # Reset row names
#'   resetRowNames(persons.ordered)
#'   
resetRowNames <- function(x)
{
  if (length(dim(x)) != 2) {
    stop(deparse(substitute(x)), " must be have two dimensions", call. = FALSE)
  }

  row.names(x) <- NULL
  x
}

# frequencyTable ---------------------------------------------------------------

#' Number of value occurrences in columns
#' 
#' Counts the number of occurrences of the different values in each column of a
#' data frame
#' 
#' @param data data frame
#' @param columns columns of \code{data} to be included in the frequency
#'   analysis. Default: all columns of \code{data}
#' @param orderByLeastLevels if TRUE (default) the list elements in the output
#'   list each of which represents one column of \code{data} or the sections of
#'   rows in the output data frame are orderd by
#'   \code{length(unique(data[[column]]))}
#' @param as.data.frame if TRUE (default) the result is a data frame, otherwise
#'   a list (see below)
#' @param useNA passed to \code{table} see there. Default: "ifany"
#'   
#' @return for \code{as.data.frame = FALSE} a list of data frames each of which 
#'   represents the frequency statistics for one column of \code{data}. Each 
#'   data frame has columns \emph{column} (name of the column of \code{data}),
#'   \emph{value} (value occurring in \emph{column} of \code{data}),
#'   \emph{count} (number of occurrences). For \code{as.data.frame = TRUE} one
#'   data frame being the result of \code{rbind}-ing together these data frames.
#'   
#' @examples 
#'   # Some example data
#'   (data <- data.frame(
#'     A = c("a1", "a2", "a1", "a1", "a2", "", "a2", NA, "a1"),
#'     B = c("b1", "b1", NA, "b2", "b2", "b1", " ", "b3", "b2")
#'   ))
#'   
#'   frequencyTable(data) # results in a data frame
#'   
#'   frequencyTable(data, as.data.frame = FALSE) # results in a list
#'   
frequencyTable <- function(
  data, columns = names(data), orderByLeastLevels = TRUE, as.data.frame = TRUE,
  useNA = c("no", "ifany", "always")[2]
)
{
  L <- .frequencyTableList(data, columns = columns, useNA = useNA)
  
  if (isTRUE(orderByLeastLevels)) {
    L <- L[order(sapply(L, nrow))]
  }
  
  if (isTRUE(as.data.frame)) {
    rbindAll(L)
  } else {
    L
  }
}

# .frequencyTableList ----------------------------------------------------------

#' .frequencyTableList
#' 
.frequencyTableList <- function(data, columns = names(data), useNA = "ifany")
{
  stopifnot(is.data.frame(data))
  checkForMissingColumns(data, columns)

  L <- lapply(columns, FUN = function(column) {

    count <- sort(table(data[[column]], useNA = useNA), decreasing = TRUE)

    # count may be empty if the column contains only NA
    if (length(count) > 0) {
      data.frame(
        column = column,
        value = names(count),
        count = as.integer(count),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  })

  structure(L, names = columns)
}

# .test_compareDataFrames ------------------------------------------------------

#' .test compareDataFrames
#' 
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

#' Compare two data frames by columns
#' 
#' @param x first data frame
#' @param y second data frame
#' 
#' @return list of logical
#' 
#' @examples 
#'   x <- data.frame(a = 1:2, b = 2:3)
#'   y <- x
#'   
#'   test1 <- all(unlist(compareDataFrames(x, y)))
#'   
#'   z <- compareDataFrames(x, y[, c("b", "a")])
#'   expectedFalse <- c("identical", "identicalExceptAttributes", "sameColumnNames")
#'   test2 <- all(names(which(!unlist(z))) == expectedFalse)
#'   
#'   test1 && test2
#'   
compareDataFrames <- function(x, y)
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
}

#
# Functions on data frames: row-related ----------------------------------------
#

# atLeastOneRowIn --------------------------------------------------------------

#' At least one row in data frame
#' 
#' returns TRUE if data frame has at least one row, else FALSE
#' 
#' @param dframe data frame
#' 
atLeastOneRowIn <- function(dframe)
{
  nrow(dframe) > 0
}

# .test_rbindAll ---------------------------------------------------------------

#' .test rbindAll
#' 
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

#' rbind all data frames given in a list
#' 
#' @param x list of data frames to be passed to \code{rbind}
#' @param nameColumn optional. If given, an additional column of that name is added to the
#'   resulting data frame containing the name (or number if \emph{args} is
#'   an unnamed list) of the element in \emph{x} that the corresponding rows
#'   belong to
#' @param remove.row.names if TRUE (default) row names are reset in the output data frame
#' @param namesAsFactor if TRUE (default) and \emph{nameColumn} is given the values in
#'   column \emph{nameColumn} are converted to a factor
#' 
#' @examples 
#'   L <- list(
#'     A = data.frame(x = 1:2, y = 2:3),
#'     B = data.frame(x = 1:3, y = 2:4)
#'   )
#'   
#'   L.unnamed <- L
#'   names(L.unnamed) <- NULL
#'   
#'   y1 <- rbindAll(L)
#'   y2 <- rbindAll(L, nameColumn = "group")
#'   y3 <- rbindAll(L.unnamed, nameColumn = "group", namesAsFactor = FALSE)
#'   y4 <- rbindAll(L.unnamed, nameColumn = "group")
#'   
#'   expected1 <- data.frame(
#'     x = c(L$A$x, L$B$x),
#'     y = c(L$A$y, L$B$y)
#'   )
#'   
#'   expected2 <- cbind(
#'     expected1,
#'     group = as.factor(c(rep("A", nrow(L$A)), rep("B", nrow(L$B)))),
#'     stringsAsFactors = FALSE
#'   )
#'   
#'   expected3 <- cbind(
#'     expected1,
#'     group = c(rep(1L, nrow(L$A)), rep(2L, nrow(L$B)))
#'   )
#'   
#'   expected4 <- expected3
#'   expected4$group <- as.factor(expected4$group)
#'   
#'   identical(y1, expected1) &&
#'     identical(y2, expected2) &&
#'     identical(y3, expected3) &&
#'     identical(y4, expected4)
#'   
rbindAll <- function(x, nameColumn = "", remove.row.names = TRUE, namesAsFactor = TRUE)
{
  result <- do.call(rbind, x)
  
  if (nameColumn != "") {
    
    xnames <- names(x)
    
    if (is.null(xnames)) {
      xnames <- seq_len(length(x))
    }
    
    times <- sapply(x, FUN = function(x) {
      if (is.null(x)) {
        0
      } else {
        nrow(x)
      }
    })
    
    nameValues <- rep(xnames, times = times)
    
    if (namesAsFactor) {
      nameValues <- as.factor(nameValues)
    }
    
    result[[nameColumn]] <- nameValues
  }
  
  if (remove.row.names) {
    row.names(result) <- NULL
  }
  
  result
}

# safeRowBindOfListElements ----------------------------------------------------

#' row-bind data frames in a list of lists
#' 
#' @param x list of lists each of which contains a data frame in element
#'   \emph{elementName}
#' @param elementName name of list element in each sublist of \emph{x} which contains a
#'   data frame
#' 
#' @return data frame resulting from "row-binding" data frames.
#' 
#' @examples 
#'   x <- list(
#'     list(
#'       number = 1,
#'       data = data.frame(x = 1:2, y = 2:3)
#'     ),
#'     list(
#'       number = 2,
#'       data = data.frame(x = 11:12, y = 12:13)
#'     )
#'   )
#'   
#'   safeRowBindOfListElements(x, "data")
#'   
#'   ## also working if the column names of the data frames in the "data" elements
#'   ## differ.
#'   x[[1]]$data$z = 13:14
#'   safeRowBindOfListElements(x, "data")
#'
safeRowBindOfListElements <- function(x, elementName)
{
  x.list <- lapply(x, "[[", elementName)
  
  result <- NULL
  
  for (dataFrame in x.list) {
    result <- safeRowBind(result, dataFrame)
  }
  
  result
}

# safeRowBind ------------------------------------------------------------------

#' "safe" rbind
#' 
#' rbind two data frames even if column names differ
#' 
#' @param dataFrame1 first data frame 
#' @param dataFrame2 second data frame
#' @examples 
#'   kwb.utils::safeRowBind(data.frame(A = 1:2, B = 2:3),
#'                          data.frame(B = 3:4, C = 4:5))
#'   
safeRowBind <- function(dataFrame1, dataFrame2)
{
  stopifnot((is.null(dataFrame1) || is.data.frame(dataFrame1)) &&
              (is.null(dataFrame2) || is.data.frame(dataFrame2)))

  if (is.null(dataFrame1)) {
    return(dataFrame2)
  }

  if (is.null(dataFrame2)) {
    return(dataFrame1)
  }

  allColumnNames <- unique(c(names(dataFrame1), names(dataFrame2)))

  dataFrame1 <- hsAddMissingCols(dataFrame1, allColumnNames)
  dataFrame2 <- hsAddMissingCols(dataFrame2, allColumnNames)

  rbind(dataFrame1, dataFrame2)
}

# safeRowBindAll ---------------------------------------------------------------

#' "safe" rbind of all data frames in a list
#' 
#' rbind all data frames in a list using \code{\link{safeRowBind}}
#' 
#' @param x list of data frames
#' 
#' @return data frame resulting from "rbind"-ing all data frames in \code{x}
#' 
safeRowBindAll <- function(x)
{
  result <- NULL

  for (element in x) {
    result <- safeRowBind(dataFrame1 = result, dataFrame2 = element)
  }

  result
  ### data frame resulting from "rbind"-ing all data frames in \code{x}
}

# addRowWithName ---------------------------------------------------------------

#' Add a Row with a Name
#' 
#' add row to data frame and give a row name at the same time
#' 
#' @param x data frame to which row is to be appended
#' @param y data frame containing the row to be appended (exacly one row expected)
#' @param row.name name of row to be given in result data frame
#' 
#' @return \emph{x} with row of \emph{y} (named \emph{row.name}) appended to it
#' 
addRowWithName <- function(x, y, row.name)
{
  stopifnot(nrow(y) == 1)

  x <- rbind(x, y)
  row.names(x)[nrow(x)] <- row.name

  return(x)
  ### \emph{x} with row of \emph{y} (named \emph{row.name}) appended to it
}

# unmerge ----------------------------------------------------------------------
#' Invert the Merging of two Data Frames
#' 
#' Split a data frame \code{z} into two data frames \code{x} and \code{y} so
#' that \code{merge(x, y)} is \code{z}.
#' 
#' @param z data frame
#' @param by vector of names of columns in \code{z} that are used to build 
#' groups of rows of \code{z} so that within each group the values in these 
#' columns do not change. For each group the columns being constant over all
#' rows are identified. Columns that are constant in each group will appear 
#' in the data frame \code{x} whereas the remaining columns will appear in the
#' data frame \code{y} of the returned list.
#' @return list with two elements \code{x} and \code{y} each of which are data 
#' frames containing at least the columns given in \code{by}.
#' @examples 
#' z <- data.frame(
#'   name = c("peter", "peter", "paul", "mary", "paul", "mary"),
#'   age = c(42, 42, 31, 28, 31, 28),
#'   height = c(181, 181, 178, 172, 178, 172),
#'   subject = c("maths", "bio", "bio", "bio", "chem", "maths"),
#'   year = c(2016, 2017, 2017, 2017, 2015, 2016),
#'   mark = c("A", "B", "B", "A", "C", "b")
#' )
#'   
#' # What fields seem to be properties of objects identified by name?
#' # -> Age and height are fix properties of the persons identified by name
#' (result1 <- unmerge(z, "name"))
#' 
#' # What fields seem to be properties of objects identified by subject?
#' # -> It seems that the subjects have been tested in different years
#' (result2 <- unmerge(z, "subject"))
#' 
#' # Test if merge(result$x, result$y) results in z
#' y1 <- merge(result1$x, result1$y)
#' y2 <- merge(result2$x, result2$y)
#' 
#' columns <- sort(names(z))
#' 
#' identical(fullySorted(z[, columns]), fullySorted(y1[, columns])) # TRUE
#' identical(fullySorted(z[, columns]), fullySorted(y2[, columns])) # TRUE
unmerge <- function(z, by)
{
  groups <- split(z, kwb.utils::selectColumns(z, by, drop = FALSE))
  
  fixColumnList <- kwb.utils::excludeNULL(lapply(groups, function(x) {
    if (nrow(x) > 0) {
      y <- kwb.utils::removeColumns(x, by)
      names(y)[sapply(y, kwb.utils::allAreEqual)]
    }
  }))
  
  fixColumns <- Reduce(intersect, fixColumnList, init = names(z))
  
  xColumns <- c(by, fixColumns)
  yColumns <- c(by, setdiff(names(z), c(by, fixColumns)))
  
  xdata <- unique(kwb.utils::selectColumns(z, xColumns, drop = FALSE))
  ydata <- kwb.utils::selectColumns(z, yColumns, drop = FALSE)
  
  stopifnot(identical(ydata, unique(ydata)))
  
  list(x = xdata, y = ydata)
}

# mergeAll ---------------------------------------------------------------------

#' Merge multiple Data Frames
#' 
#' Merge multiple data frames, given in a list
#' 
#' @param dataFrames list of data frames. If the list elements are named, the
#'   element names are used as suffixes in the column names, otherwise suffixes
#'   ".1", ".2", etc are used
#' @param by vector of column names to be merged by, passed on to \code{merge}
#' @param \dots additional arguments passed to \code{merge}
#' @param dbg if \code{TRUE} (default) debug messages showing the process of 
#'   merging are shown
#'   
#' @return data frame being the result of merging all the data frames given in 
#'   \emph{dataFrames} by  consecutively calling \code{merge}
#'   
#' @examples 
#'   peter <- data.frame(fruit = c("apple", "pear", "banana"), kg = 1:3)
#'   paul <- data.frame(fruit = c("banana", "apple", "lemon"), kg = c(10, 20, 30))
#'   mary <- data.frame(fruit = c("lemon", "organger", "apple"), kg = c(22, 33, 44))
#'   
#'   # By default only categories that are in all data frames are returned
#'   mergeAll(list(peter = peter, paul = paul, mary = mary), by = "fruit")
#'   
#'   # Use the arguments supported by merge to change that behaviour
#'   mergeAll(list(peter = peter, paul = paul, mary = mary), by = "fruit", all = TRUE)
#'   
mergeAll <- function(dataFrames, by, ..., dbg = TRUE)
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
    
    catIf(dbg, "merging table", i, "/", max(indices), "... ")
    
    # Append the respective suffix to the non-key columns
    names(dataFrames[[i]]) <- appendSuffix(
      values = names(dataFrames[[i]]),
      suffix = suffixes[i],
      valuesToOmit = by
    )
    
    # Merge the current data frame to the result data frame
    result <- merge(result, dataFrames[[i]], by = by, ...)
    
    catIf(dbg, "ok.\n")
  }
  
  result
}

# safeMerge --------------------------------------------------------------------

#' Merge By Checking Column Existence
#' 
#' check existence of columns before merging
#' 
#' @param x just as in \code{\link[base]{merge}}
#' @param y just as in \code{\link[base]{merge}}
#' @param by just as in \code{\link[base]{merge}}
#' @param by.x just as in \code{\link[base]{merge}}
#' @param by.y just as in \code{\link[base]{merge}}
#' @param \dots additional arguments passed to \code{\link[base]{merge}}
#' 
safeMerge <- function(
  x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, ...
)
{
  checkForMissingColumns(x, by.x)
  checkForMissingColumns(y, by.y)

  merge(x, y, by = by, by.x = by.x, by.y = by.y, ...)
}

# moveToFront ------------------------------------------------------------------

#' Move elements to the start of a vector
#' 
#' @param x vector
#' @param elements elements out of \code{x} to be moved to the front
#' 
#' @return vector with \code{elements} coming first
#' 
#' @examples 
#'   moveToFront(1:10, 5)
#'   moveToFront(c("a", "b", "c", "x", "y", "d"), c("x", "y"))
#'   
moveToFront <- function(x, elements)
{
  c(elements, setdiff(x, elements))
}

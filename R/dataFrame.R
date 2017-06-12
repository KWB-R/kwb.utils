# pasteColumns0 ----------------------------------------------------------------

#' Paste Columns of Data Frame without Separator
#' 
#' paste the columns of a data frame without a separator
#' 
#' @param x data frame
#' @param columns names of columns to be pasted. Default: all columns
#' @param \dots args passed to \code{\link{pasteColumns}}
#' 
#' @return vector of character with each element representing the values of the
#'   selected columns of one row, being pasted without a separator
#' 
#' @examples 
#'   x <- data.frame(A = 1:3, B = 2:4)
#'   pasteColumns0(x)
#'   
#' 
pasteColumns0 <- structure(
  function # Paste Columns of Data Frame without Separator
  ### paste the columns of a data frame without a separator
  (
    x,
    ### data frame
    columns = names(x),
    ### names of columns to be pasted. Default: all columns
    ...
    ### args passed to \code{\link{pasteColumns}}
  )
  {
    pasteColumns(x, columns, sep = "", ...)
    ### vector of character with each element representing the values of the
    ### selected columns of one row, being pasted without a separator
  }, ex = function() {
    x <- data.frame(A = 1:3, B = 2:4)
    pasteColumns0(x)
  })

# pasteColumns -----------------------------------------------------------------

#' paste the columns of a data frame with a separator
#' 
#' paste the columns of a data frame with a separator
#' 
#' @param x data frame
#' @param columns names of columns to be pasted. Default: all columns
#' @param sep separator character. Default: space (" ")
#' @param \dots args passed to \code{\link{selectColumns}}, e.g. \code{do.stop} to control
#'   whether the function shall stop if not all columns exist
#' 
#' @return vector of character with each element representing the values of the
#'   selected columns of one row, being pasted with the separator character
#' 
#' @examples 
#'   x <- data.frame(A = 1:3, B = 2:4)
#'   pasteColumns(x, sep = ";")
#'   
#' 
pasteColumns <- structure(
  function # paste the columns of a data frame with a separator
### paste the columns of a data frame with a separator
(
  x,
  ### data frame
  columns = names(x),
  ### names of columns to be pasted. Default: all columns
  sep = " ",
  ### separator character. Default: space (" ")
  ...
  ### args passed to \code{\link{selectColumns}}, e.g. \code{do.stop} to control
  ### whether the function shall stop if not all columns exist
)
{
  if (length(columns) > 1) {
    args <- selectColumns(x, columns, ...)
    do.call(paste, c(args, sep = sep))
  } else {
    selectColumns(x, columns, ...)
  }
  ### vector of character with each element representing the values of the
  ### selected columns of one row, being pasted with the separator character
}, ex = function() {
  x <- data.frame(A = 1:3, B = 2:4)
  pasteColumns(x, sep = ";")
})

# splitIntoFixSizedBlocks ------------------------------------------------------

#' split into blocks of same size
#' 
#' split a data frame or matrix into blocks of the same size (= data frames of
#'   matrices with the same number ofrows)
#' 
#' @param data data frame or matrix
#' @param blocksize number of rows in each block into which \code{data} is split
#' 
#' @return list of data frames (if \code{data} is a data frame) or list of matrices
#'   (if \code{data} is a matrix)
#' 
splitIntoFixSizedBlocks <- function # split into blocks of same size
### split a data frame or matrix into blocks of the same size (= data frames of
### matrices with the same number ofrows)
(
  data,
  ### data frame or matrix
  blocksize
  ### number of rows in each block into which \code{data} is split
)
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
#'   # Resetted row names
#'   resetRowNames(persons.ordered)
#'   
#' 
resetRowNames <- structure(function # Reset row names to 1:n
### Reset the row names of a data frame x to 1:nrow(x) by setting the
### \code{row.names} attribute to \code{NULL}.
(
  x
  ### data frame or matrix
)
{
  if (length(dim(x)) != 2) {
    stop(deparse(substitute(x)), " must be have two dimensions", call. = FALSE)
  }

  row.names(x) <- NULL
  x
}, ex = function() {
  persons <- data.frame(id = c(1, 2, 3), name = c("Peter", "Paul", "Mary"))

  persons.ordered <- persons[order(persons$name), ]

  # Original row names
  persons.ordered

  # Resetted row names
  resetRowNames(persons.ordered)
})

# frequencyTable ---------------------------------------------------------------

#' number of value occurrences in columns
#' 
#' Counts the number of occurrences of the different values in each
#'   column of a data frame
#' 
#' @param data data frame
#' @param columns columns of \code{data} to be included in the frequency analysis.
#'   Default: all columns of \code{data}
#' @param orderByLeastLevels if TRUE (default) the list elements in the output list each of which
#'   represents one column of \code{data} or the sections of rows in the
#'   output data frame are orderd by \code{length(unique(data[[column]]))}
#' @param as.data.frame if TRUE (default) the result is a data frame, otherwise a list (see
#'   below)
#' @param useNA passed to \code{table} see there. Default: "ifany"
#' 
#' @return for \code{as.data.frame = FALSE} a list of data frames each of which
#'   represents the frequency statistics for one column of \code{data}. Each
#'   data frame has columns \emph{column} (name of the column of
#'   \code{data}), \emph{value} (value occurring in \emph{column} of
#'   \code{data}), \emph{count} (number of occurrences). For
#'   \code{as.data.frame = TRUE} one data frame being the result of
#'   \code{rbind}-ing together these data frames.
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
#' 
frequencyTable <- structure(
  function # number of value occurrences in columns
  ### Counts the number of occurrences of the different values in each
  ### column of a data frame
  (
    data,
    ### data frame
    columns = names(data),
    ### columns of \code{data} to be included in the frequency analysis.
    ### Default: all columns of \code{data}
    orderByLeastLevels = TRUE,
    ### if TRUE (default) the list elements in the output list each of which
    ### represents one column of \code{data} or the sections of rows in the
    ### output data frame are orderd by \code{length(unique(data[[column]]))}
    as.data.frame = TRUE,
    ### if TRUE (default) the result is a data frame, otherwise a list (see
    ### below)
    useNA = c("no", "ifany", "always")[2]
    ### passed to \code{table} see there. Default: "ifany"
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
    ### for \code{as.data.frame = FALSE} a list of data frames each of which
    ### represents the frequency statistics for one column of \code{data}. Each
    ### data frame has columns \emph{column} (name of the column of
    ### \code{data}), \emph{value} (value occurring in \emph{column} of
    ### \code{data}), \emph{count} (number of occurrences). For
    ### \code{as.data.frame = TRUE} one data frame being the result of
    ### \code{rbind}-ing together these data frames.
  }, ex = function() {
    # Some example data
    (data <- data.frame(
      A = c("a1", "a2", "a1", "a1", "a2", "", "a2", NA, "a1"),
      B = c("b1", "b1", NA, "b2", "b2", "b1", " ", "b3", "b2")
    ))

    frequencyTable(data) # results in a data frame

    frequencyTable(data, as.data.frame = FALSE) # results in a list
  })

# .frequencyTableList ----------------------------------------------------------

#'  frequencyTableList
#' 
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

#'  test compareDataFrames
#' 
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

#' compare two data frames by columns
#' 
#' compare two data frames by columns
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
#' 
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

#' at least one row in data frame
#' 
#' returns TRUE if data frame has at least one row, else FALSE
#' 
#' @param dframe data frame
#' 
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

#'  test rbindAll
#' 
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
#' 
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
  }, ex = .test_rbindAll
)

# safeRowBindOfListElements ----------------------------------------------------

#' row-bind data frames in a list of lists
#' 
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
#' 
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
safeRowBindAll <- function # "safe" rbind of all data frames in a list
### rbind all data frames in a list using \code{\link{safeRowBind}}
(
  x
  ### list of data frames
)
{
  result <- NULL

  for (element in x) {
    result <- safeRowBind(dataFrame1 = result, dataFrame2 = element)
  }

  result
  ### data frame resulting from "rbind"-ing all data frames in \code{x}
}

# addRowWithName ---------------------------------------------------------------

#' addRowWithName
#' 
#' add row to data frame and give a row name at the same time
#' 
#' @param x data frame to which row is to be appended
#' @param y data frame containing the row to be appended (exacly one row expected)
#' @param row.name name of row to be given in result data frame
#' 
#' @return \emph{x} with row of \emph{y} (named \emph{row.name}) appended to it
#' 
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

#' merge multiple data frames
#' 
#' merge multiple data frames, given in a list
#' 
#' @param dataFrames list of data frames. If the list elements are named, the element names
#'   are used as suffixes in the column names, otherwise suffixes ".1", ".2",
#'   etc are used
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
safeMerge <- function
(
  x,
  y,
  by = intersect(names(x), names(y)),
  by.x = by,
  by.y = by,
  ...
)
{
  checkForMissingColumns(x, by.x)
  checkForMissingColumns(y, by.y)

  merge(x, y, by = by, by.x = by.x, by.y = by.y, ...)
}

# safeColumnBind ---------------------------------------------------------------

#' "Safe" version of cbind.
#' 
#' If \code{x1} is NULL \code{x2} is returned otherwise \code{cbind(x1, x2)}
#' 
#' @param x1 first object to be passed to \code{cbind}
#' @param x2 second object to be passed to  \code{cbind}
#' 
#' @return result of \code{cbind(x1, x2)} or \code{x2} if \code{x1}
#'   is \code{NULL}.
#' 
#' @examples 
#'   x1 <- NULL
#'   
#'   for (i in 1:3) {
#'   
#'     x2 <- data.frame(a = 1:3, b = rnorm(3))
#'     x1 <- safeColumnBind(x1, x2)
#'   
#'     # using cbind would result in an error:
#'     # x1 <- cbind(x1, x2)
#'   }
#'   
#'   x1
#'   
safeColumnBind <- function(x1, x2)
{
  if (is.null(x1)) {
    x2
  }
  else {
    cbind(x1, x2)
  }
  ### result of \code{cbind(x1, x2)} or \emph{x2} if \emph{x1}
  ### is null.
}

# posixColumnAtPosition --------------------------------------------------------

#' posixColumnAtPosition
#' 
#' posixColumnAtPosition
#' 
#' @param x data frame containing a date/time column
#' 
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

#' data/time column of data frame
#' 
#' data/time column of data frame
#' 
#' @param x data frame in which to find a column of class "POSIXt"
#' 
firstPosixColumn <- function(x)
{
  stopifnot (is.data.frame(x))

  x[[posixColumnAtPosition(x)]]
}

# roundColumns -----------------------------------------------------------------

#' roundColumns
#' 
#' roundColumns
#' 
#' @param dframe data frame containing numeric columns to be rounded
#' @param columnNames names of (numeric) columns in \emph{dframe} to be rounded.
#' @param digits number of digits to be rounded to (vector of length 1 expected) or list of
#'   assignments in the form: \emph{columnName} = \emph{numberOfDigits}. If
#'   you give a list here, then there is no need to set the argument
#'   \emph{columnNames}
#' 
#' @return \emph{dframe} with columns given in \emph{columnNames} being rounded to
#'   \emph{digits} digits.
#' 
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

# selectColumns ----------------------------------------------------------------

#' select columns from data frame
#' 
#' select columns from data frame. Stop with message if columns do not exist
#' 
#' @param x data frame
#' @param columns vector of column names. If \code{columns} is of length 0 or \code{NULL}
#'   (default) or \code{NA} \code{x} is returned unchanged.
#' @param drop if \code{TRUE} and if only one column is to be selected the result is a
#'   vector (one dimensional) containing the values of the selected column and
#'   not a data frame. One dimension has been \emph{dropped} then. See the
#'   \code{help("[.data.frame")}. The default is \code{TRUE} if
#'   \code{length(columns) == 1}, else \code{FALSE}.
#' @param do.stop this flag controls whether the function stops (\code{do.stop = TRUE}) or
#'   not (\code{do.stop = FALSE}) if there are non-existing columns to be
#'   selected. If \code{do.stop = FALSE} only those columns are selected that
#'   actually exist
#' 
#' @return data frame containing the columns of \code{x} that are specified in
#'   \code{columns} or \code{x} itself if \code{columns} is \code{NULL} or
#'   a vector containing the values of column \code{value} if \code{columns} is
#'   of length 1 and \code{drop = TRUE} (which is the default in this case).
#' 
selectColumns <- function # select columns from data frame
### select columns from data frame. Stop with message if columns do not exist
(
  x,
  ### data frame
  columns = NULL,
  ### vector of column names. If \code{columns} is of length 0 or \code{NULL}
  ### (default) or \code{NA} \code{x} is returned unchanged.
  drop = (length(columns) == 1),
  ### if \code{TRUE} and if only one column is to be selected the result is a
  ### vector (one dimensional) containing the values of the selected column and
  ### not a data frame. One dimension has been \emph{dropped} then. See the
  ### \code{help("[.data.frame")}. The default is \code{TRUE} if
  ### \code{length(columns) == 1}, else \code{FALSE}.
  do.stop = TRUE
  ### this flag controls whether the function stops (\code{do.stop = TRUE}) or
  ### not (\code{do.stop = FALSE}) if there are non-existing columns to be
  ### selected. If \code{do.stop = FALSE} only those columns are selected that
  ### actually exist
)
{
  stopifnot(is.data.frame(x))

  if (is.null(columns) || length(columns) == 0 || all(is.na(columns))) {
    return(x)
  }

  ok <- checkForMissingColumns(
    x, columns, dataFrameName = deparse(substitute(x)), do.stop = do.stop
  )

  if (! ok) {
    warning("Only the existing columns are selected.")
    columns <- intersect(columns, names(x))
  }

  x[, columns, drop = drop]
  ### data frame containing the columns of \code{x} that are specified in
  ### \code{columns} or \code{x} itself if \code{columns} is \code{NULL} or
  ### a vector containing the values of column \code{value} if \code{columns} is
  ### of length 1 and \code{drop = TRUE} (which is the default in this case).
}

# moveToFront ------------------------------------------------------------------

#' move elements to the start of a vector
#' 
#' move elements to the start of a vector
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
#' 
moveToFront <- structure(
  function # move elements to the start of a vector
### move elements to the start of a vector
(
  x,
  ### vector
  elements
  ### elements out of \code{x} to be moved to the front
)
{
  c(elements, setdiff(x, elements))
  ### vector with \code{elements} coming first
}, ex = function() {
  moveToFront(1:10, 5)
  moveToFront(c("a", "b", "c", "x", "y", "d"), c("x", "y"))
})

# moveColumnsToFront -----------------------------------------------------------

#' move columns to the start of a data frame
#' 
#' move columns to the start of a data frame or matrix
#' 
#' @param x data frame
#' @param columns vector of column names
#' 
#' @return data frame or matrix with \code{columns} being the leftmost columns
#' 
#' @examples 
#'   x <- data.frame(a = 1:5, b = 2:6, c = 3:7)
#'   
#'   moveColumnsToFront(x, "b")
#'   moveColumnsToFront(x, c("b", "a"))
#'   
#' 
moveColumnsToFront <- structure(
  function # move columns to the start of a data frame
### move columns to the start of a data frame or matrix
(
  x,
  ### data frame
  columns = NULL
  ### vector of column names
)
{
  selectColumns(x, moveToFront(names(x), columns))
  ### data frame or matrix with \code{columns} being the leftmost columns
}, ex = function() {
  x <- data.frame(a = 1:5, b = 2:6, c = 3:7)

  moveColumnsToFront(x, "b")
  moveColumnsToFront(x, c("b", "a"))
})

# checkForMissingColumns -------------------------------------------------------

#' Check for column existence
#' 
#' Stops if data frame \emph{frm} does not contain all columns of which the
#'   names are given in \emph{reqCols}.
#' 
#' @param frm data frame
#' @param reqCols vector of names of which existence in \emph{frm} shall be checked
#' @param do.stop if TRUE, stop() is called else warning() if a column is missing
#' @param dataFrameName the name of the data frame to be shown in the error message if a
#'   column was missing
#' 
#' @return TRUE if all required columns are available, else FALSE
#' 
checkForMissingColumns <- function # Check for column existence
### Stops if data frame \emph{frm} does not contain all columns of which the
### names are given in \emph{reqCols}.
(
  frm,
  ### data frame
  reqCols,
  ### vector of names of which existence in \emph{frm} shall be checked
  do.stop = TRUE,
  ### if TRUE, stop() is called else warning() if a column is missing
  dataFrameName = deparse(substitute(frm))
  ### the name of the data frame to be shown in the error message if a
  ### column was missing
)
{
  columnNames <- names(frm)

  missing <- reqCols[!(reqCols %in% columnNames)]

  if (!isNullOrEmpty(missing)) {

    infotext <- paste(
      sprintf("%d column%s missing in data frame '%s': %s",
              length(missing), ifelse(length(missing) > 1, "s", ""),
              dataFrameName, collapsed(hsQuoteChr(missing), ", ")),
      sprintf("Available column%s: %s",
              ifelse(length(columnNames) > 1, "s", ""),
              collapsed(hsQuoteChr(columnNames), ", ")),
      sep = "\n"
    )

    do.call(ifelse(do.stop, "stop", "warning"), list(infotext, call. = FALSE))
  }

  return (isNullOrEmpty(missing))
  ### TRUE if all required columns are available, else FALSE
}

# hsAddMissingCols -------------------------------------------------------------

#' Add missing columns to data frame
#' 
#' Adds missing columns to the given data frame so that the resulting
#'   data frame contains all columns given in the vector \emph{colNames}.
#'   Added columns are filled with NA values.
#' 
#' @param dataFrame data frame to which column names are to be appended
#' @param colNames vector containing names of columns that shall be contained in
#'   \emph{dataFrame}
#' @param fill.value value to be inserted into newly created columns. Default: NA
#' 
#' @return data frame with columns as listed in \emph{colNames}
#' 
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

#' Delete empty columns of data frame
#' 
#' Returns data frame in which all empty columns (NA in all rows) are removed
#' 
#' @param dataFrame data frame of which empty columns (NA in all rows) are to be removed
#' @param FUN function to be applied to each column to decide whether the column
#'   is empty or not. Default: \code{function(x) all(is.na(x))}
#' @param drop if \code{TRUE} (the default is \code{FALSE}) one dimension is 
#'   dropped (a vector is returned instead of a data frame) in case that all but
#'    one columns are removed.  
#' @return copy of input data frame but with all empty columns removed
#' 
#' @seealso \code{\link{removeEmptyColumns}}
#' 
hsDelEmptyCols <- function
(
  dataFrame,
  FUN = function(x) all(is.na(x)),
  drop = FALSE
)
{
  isEmpty <- sapply(dataFrame, FUN)

  dataFrame[, ! isEmpty, drop = drop]
}

# removeEmptyColumns -----------------------------------------------------------

#' Remove empty columns from a data frame
#' 
#' Remove empty columns from a data frame.
#' 
#' @param x data frame
#' @param drop if \code{TRUE} and only one column remains the column is returned
#'   as a vector
#' @param FUN function called on each column to determine if all values in the
#'   column are empty. Default: \code{function(x) all(is.na(x))}
#' @param dbg if \code{TRUE} debug messages are shown
#' 
#' @return data frame \code{x} with empty columns (columns with NA in all rows) 
#'   being removed
#'   
#' @seealso \code{\link{hsDelEmptyCols}}
#' 
removeEmptyColumns <- function # Remove empty columns from a data frame
### Remove empty columns from a data frame.
(
  x,
  drop = FALSE,
  FUN = function(x) all(is.na(x)),
  dbg = TRUE
)
{
  objectName <- as.character(substitute(x))

  isEmpty <- sapply(x, FUN)

  if (any(isEmpty)) {
    
    catIf(dbg, sprintf("%s: %d empty columns removed: %s\n",
                       objectName,
                       sum(isEmpty),
                       paste(names(x)[isEmpty], collapse = ", ")))
    
  } else {
    
    catIf(dbg, sprintf("%s: No empty columns.\n", objectName))
  }

  x[, ! isEmpty, drop = drop]
}

# removeColumns ----------------------------------------------------------------

#' remove columns from data frame
#' 
#' remove columns from a data frame
#' 
#' @param dframe data frame,
#' @param columnsToRemove vector of column names giving the columns to remove
#' @param drop if FALSE, a data frame is returned in any case, otherwise the result may
#'   be a vector if only one column remains
#' 
#' @return \emph{dframe} with columns given in \emph{columnsToRemove} being removed.
#'   User attributes of \emph{dframe} are restored.
#' 
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

# insertColumns ----------------------------------------------------------------

#' insert new column(s) into data frame
#' 
#' insert one or more new columns into a data frame before or after the given
#'   column
#' 
#' @param Data data frame
#' @param \dots named objects each of which will be a new column in the data frame.
#'   Each object must have as many elements as Data has rows.
#' @param before name of column before which to insert the new column(s)
#' @param after name of column after which to insert the new column(s)
#' @param stringsAsFactors passed on to data.frame() and cbind()
#' 
#' @return data frame \code{Data} with new columns inserted before the column named
#'   as given in \code{before} or after the column named as given in
#'   \code{after}
#' 
#' @examples 
#'   Data <- data.frame(A = 1:5, B = 2:6)
#'   
#'   # Insert new columns X and Y before column "B"
#'   insertColumns(Data, before = "B", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#'   
#'   # This is the same as inserting new columns X and Y after column "A":
#'   insertColumns(Data, after = "A", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#'   
#'   # You may also insert before the first...
#'   insertColumns(Data, before = "A", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#'   
#'   # ... or after the last column
#'   insertColumns(Data, after = "B", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#'   
#' 
insertColumns <- structure(
  function # insert new column(s) into data frame
### insert one or more new columns into a data frame before or after the given
### column
(
  Data,
  ### data frame
  ...,
  ### named objects each of which will be a new column in the data frame.
  ### Each object must have as many elements as Data has rows.
  before = "",
  ### name of column before which to insert the new column(s)
  after = "",
  ### name of column after which to insert the new column(s)
  stringsAsFactors = default.stringsAsFactors()
  ### passed on to data.frame() and cbind()
)
{
  stopifnot(is.data.frame(Data))
  stopifnot(is.character(before) && length(before) == 1)
  stopifnot(is.character(after) && length(after) == 1)

  # Exactly one of before or after must be given
  if (sum(c(before, after) != "") != 1) {
    stop("Exactly one of before and after must be given!")
  }

  # The reference column (named in before or after) must be given; get its index
  refColumn <- ifelse(before != "", before, after)
  checkForMissingColumns(Data, refColumn)

  # All new columns must be named
  newColumns <- list(...)
  columnNames <- names(newColumns)

  if (is.null(columnNames) ||
      length(columnNames[columnNames != ""]) != length(newColumns)) {
    stop("All new columns must be named, i.e. given in the form 'name = values'")
  }

  # All new columns must be of equal length
  equalLength <- (sapply(newColumns, length) == nrow(Data))

  if (! all(equalLength)) {
    stop("All new columns must have as many elements as Data has rows. ",
         "This is not the case for: ", commaCollapsed(columnNames[! equalLength]))
  }

  i <- which(refColumn == names(Data))

  n.col <- ncol(Data)
  partBetween <- data.frame(..., stringsAsFactors = stringsAsFactors)

  if (before != "") {
    part1 <- if (i == 1) {
      partBetween
    }
    else {
      cbind(Data[, 1:(i-1), drop = FALSE], partBetween)
    }
    part2 <- Data[, i:n.col, drop = FALSE]
  } else {
    part1 <- Data[, 1:i, drop = FALSE]
    part2 <- if (i == n.col) {
      partBetween
    } else {
      cbind(
        partBetween,
        Data[, (i+1):n.col, drop = FALSE],
        stringsAsFactors = stringsAsFactors
      )
    }
  }

  cbind(part1, part2, stringsAsFactors = stringsAsFactors)
  ### data frame \code{Data} with new columns inserted before the column named
  ### as given in \code{before} or after the column named as given in
  ### \code{after}
}, ex = function() {
  Data <- data.frame(A = 1:5, B = 2:6)

  # Insert new columns X and Y before column "B"
  insertColumns(Data, before = "B", X = paste0("x", 1:5), Y = paste0("y", 1:5))

  # This is the same as inserting new columns X and Y after column "A":
  insertColumns(Data, after = "A", X = paste0("x", 1:5), Y = paste0("y", 1:5))

  # You may also insert before the first...
  insertColumns(Data, before = "A", X = paste0("x", 1:5), Y = paste0("y", 1:5))

  # ... or after the last column
  insertColumns(Data, after = "B", X = paste0("x", 1:5), Y = paste0("y", 1:5))
})

# hsRenameColumns --------------------------------------------------------------

#' rename columns in a data frame
#' 
#' rename columns in a data frame giving tupels of original name
#'   and substitute name as named elements in list "renames"
#' 
#' @param dframe data.frame,
#' @param renames list with named elements each of which defines a column rename in the form
#'   <old-name> = <new-name>
#' 
#' @return \emph{dframe} with columns renamed as specified in \emph{renames}
#' 
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

# renameAndSelect --------------------------------------------------------------

#' rename and select columns of a data frame
#' 
#' rename and select columns of a data frame
#' 
#' @param data data frame
#' @param renames list defining renames in the form of "oldName" = "newName" pairs
#' @param columns (new) names of colums to be selected
#' 
renameAndSelect <- function # rename and select columns of a data frame
### rename and select columns of a data frame
(
  data,
  ### data frame
  renames,
  ### list defining renames in the form of "oldName" = "newName" pairs
  columns = unlist(renames)
  ### (new) names of colums to be selected
)
{
  data <- kwb.utils::hsRenameColumns(data, renames)

  kwb.utils::selectColumns(data, columns, drop = FALSE)
}

# setColumns -------------------------------------------------------------------

#' Set the column(s) of a data frame
#' 
#' Set the (new or existing) column(s) of a data frame.
#' 
#' @param .x data frame
#' @param \dots column assignment(s) in the form of \code{<columnName> = <values>}
#' @param dbg if \code{TRUE} (default) the creation of new columns is reported on the
#'   screen
#' 
#' @return data frame with columns modified or appended as specified with the
#'   \code{assignments}
#' 
#' @examples 
#'   
#'   # Create a data frame
#'   x <- data.frame(a = 1:5)
#'   
#'   # Option 1: use the "$" operator
#'   x1 <- x
#'   x1$b <- 2:6
#'   x1$c <- 3:7
#'   
#'   # Option 2: use setColumns
#'   x2 <- setColumns(x, b = 2:6, c = 3:7)
#'   
#'   # The result is the same
#'   identical(x1, x2)
#'   
#'   # but the creation of columns has been reported on the console (dbg = TRUE by
#'   # default)
#'   
#'   ## Provide column 'b' to data frame 'x'... ok.
#'   ## Provide column 'c' to data frame 'x'... ok.
#'   
#' 
setColumns <- structure(function # Set the column(s) of a data frame
### Set the (new or existing) column(s) of a data frame.
(
  .x,
  ### data frame
  ...,
  ### column assignment(s) in the form of \code{<columnName> = <values>}
  dbg = TRUE
  ### if \code{TRUE} (default) the creation of new columns is reported on the
  ### screen
)
{
  stopifnot(is.data.frame(.x))

  name.x <- deparse(substitute(.x))

  assignments <- list(...)

  if (any(is.unnamed(assignments))) {
    stop("All column assignments be named!", call. = FALSE)
  }

  for (columnName in names(assignments)) {

    catIf(dbg, sprintf("Provide column '%s' to data frame '%s'... ",
                       columnName, name.x))

    .x[[columnName]] <- assignments[[columnName]]

    catIf(dbg, "ok.\n")
  }

  .x
  ### data frame with columns modified or appended as specified with the
  ### \code{assignments}
}, ex = function() {

  # Create a data frame
  x <- data.frame(a = 1:5)

  # Option 1: use the "$" operator
  x1 <- x
  x1$b <- 2:6
  x1$c <- 3:7

  # Option 2: use setColumns
  x2 <- setColumns(x, b = 2:6, c = 3:7)

  # The result is the same
  identical(x1, x2)

  # but the creation of columns has been reported on the console (dbg = TRUE by
  # default)

  ## Provide column 'b' to data frame 'x'... ok.
  ## Provide column 'c' to data frame 'x'... ok.
})

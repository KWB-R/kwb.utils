# frequencyTableList -----------------------------------------------------------

frequencyTableList <- function(data, columns = names(data), useNA = "ifany")
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

# addRowWithName ---------------------------------------------------------------

#' Add a Row with a Name
#' 
#' add row to data frame and give a row name at the same time
#' 
#' @param x data frame to which row is to be appended
#' @param y data frame containing the row to be appended (exacly one row
#'   expected)
#' @param row.name name of row to be given in result data frame
#' @return \emph{x} with row of \emph{y} (named \emph{row.name}) appended to it
#' @export
#' 
addRowWithName <- function(x, y, row.name)
{
  stopifnot(nrow(y) == 1)
  
  x <- rbind(x, y)
  
  row.names(x)[nrow(x)] <- row.name
  
  return(x)
}

# asNoFactorDataFrame ----------------------------------------------------------

#' Convert to Data Frame without Factors
#'
#' Use \code{\link{as.data.frame}} with \code{stringsAsFactors = FALSE}
#'
#' @param \dots passed to \code{\link{as.data.frame}}
#' @export
#' @examples
#' data_matrix <- matrix(LETTERS[1:6], nrow = 2)
#'
#' # as.data.frame() by default converts character to factor
#' str(as.data.frame(data_matrix))
#'
#' # asNoFactorDataFrame keeps character as character
#' str(asNoFactorDataFrame(data_matrix))
#'
asNoFactorDataFrame <- function(...)
{
  as.data.frame(..., stringsAsFactors = FALSE)
}

# atLeastOneRowIn --------------------------------------------------------------

#' At least one row in data frame
#' 
#' returns TRUE if data frame has at least one row, else FALSE
#' 
#' @param dframe data frame
#' @export
#' 
atLeastOneRowIn <- function(dframe)
{
  nrow(dframe) > 0
}

# compareDataFrames ------------------------------------------------------------

#' Compare two data frames by columns
#' 
#' @param x first data frame
#' @param y second data frame
#' @param dbg if \code{TRUE} (default) the result of comparing the dimensions and the
#'   column names is printed on the screen
#' @param xname name of first data frame to appear in the output if \code{dbg =
#'   TRUE}
#' @param yname name of second data frame to appear in the output if \code{dbg =
#'   TRUE}
#' @return list of logical
#' @export
#' @examples 
#' x <- data.frame(a = 1:2, b = 2:3)
#' y <- x
#' 
#' test1 <- all(unlist(compareDataFrames(x, y)))
#' 
#' z <- compareDataFrames(x, y[, c("b", "a")])
#' expectedFalse <- c("identical", "identicalExceptAttributes", "sameColumnNames")
#' test2 <- all(names(which(!unlist(z))) == expectedFalse)
#' 
#' test1 && test2
#'
compareDataFrames <- function(
  x, y, dbg = FALSE, xname = deparse(substitute(x)), 
  yname = deparse(substitute(y))
)
{
  stopifnot(is.data.frame(x))
  stopifnot(is.data.frame(y))
  
  catIf(dbg, sprintf("Dimension of %s: %s\n", xname, collapsed(dim(x))))
  catIf(dbg, sprintf("Dimension of %s: %s\n", yname, collapsed(dim(y))))
  
  names.x <- names(x)
  names.y <- names(y)
  
  if (dbg) {
    
    compareSets(names.x, names.y, "Columns", xname, yname)
  }
  
  row.names.x <- row.names(x)
  row.names.y <- row.names(y)
  
  result <- list()
  
  # Are the data frames identical?
  result$identical <- identical(x, y)
  
  # Are the data frames identical after removing all attributes?
  result$identicalExceptAttributes <- identical(
    removeAttributes(x),
    removeAttributes(y)
  )
  
  # Do the data frames have the same number of rows?
  result$equalNumberOfRows <- (nrow(x) == nrow(y))
  
  # Do the data frames have the same number of columns?
  result$equalNumberOfColumns <- (ncol(x) == ncol(y))
  
  typeToName <- c(Column = "names", Row = "row.names")
  
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
        removeAttributes(x[[column]]),
        removeAttributes(y[[column]])
      ),
      equalValues = all(x[[column]] == y[[column]])
    )
  })
  
  result$byColumn <- check
  
  result
}

# compareSets ------------------------------------------------------------------

#' Compare the values in two vectors
#' 
#' Prints the result of comparing the values of two vectors with each other 
#' (which values are in the first vector but not in the second one and which 
#' values are in the second vector but not in the first one) on the screen.
#' 
#' @param x first vector
#' @param y second vector
#' @param subject name of objects to be compared that will appear in the 
#'   message. Default: \code{"Values"}.
#' @param xname optional name of the first vector that will be used in the
#'   message
#' @param yname optional name of the second vector that will be used in the
#'   message
#' @export
#' @examples
#' compareSets(1:10, 3:13)
#' compareSets(1:10, 3:13, "numbers", "set 1", "set 2")
#' 
compareSets <- function(
  x, y, subject = "Values", xname = deparse(substitute(x)), 
  yname = deparse(substitute(y))
)
{
  stringFormat <- "%s in %s that are not in %s: %s\n"
  
  cat(sprintf(stringFormat, subject, xname, yname, stringList(setdiff(x, y))))
  
  cat(sprintf(stringFormat, subject, yname, xname, stringList(setdiff(y, x))))
}

# expandGrid ------------------------------------------------------------------

#' Wrapper around expand.grid
#' 
#' Same as \code{\link[base]{expand.grid}} but with \code{stringsAsFactors =
#' FALSE} by default and with the values of the first argument being changed
#' last, not first.
#' @param ... arguments passed to \code{\link[base]{expand.grid}}, but in 
#'   reversed order
#' @param stringsAsFactors passed to \code{\link[base]{expand.grid}}
#' @export
#' @examples 
#' persons <- c("Peter", "Paul", "Mary")
#' fruits <- c("apple", "pear")
#' 
#' # With expand.grid() the values of the first argument change first...
#' (grid_1 <- expand.grid(person = persons, fruit = fruits))
#' 
#' #... with expandGrid() they change last.
#' (grid_2 <- expandGrid(person = persons, fruit = fruits))
#' 
#' # With expand.grid() character strings are converted to factors by default...
#' str(grid_1)
#' 
#' # ... with expandGrid() character strings are not converted by default.
#' # Also, there is no attribute "out.attrs" as it is set by expand.grid().
#' str(grid_2)
#' 
expandGrid <- function(..., stringsAsFactors = FALSE)
{
  args_1 <- rev(list(...))
  args_2 <- list(stringsAsFactors = stringsAsFactors)
  
  grid <- do.call(expand.grid, c(args_1, args_2))

  # Unnamed arguments are given default names "Var1", "Var2", ... by expand.grid  
  # Reverse the order of these names so that "Var1" appears first
  unnamed <- is.unnamed(args_1)
  names(grid)[unnamed] <- paste0("Var", rev(seq_len(sum(unnamed))))
  
  structure(grid[, rev(seq_along(grid))])
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
#' @return for \code{as.data.frame = FALSE} a list of data frames each of which 
#'   represents the frequency statistics for one column of \code{data}. Each 
#'   data frame has columns \emph{column} (name of the column of \code{data}),
#'   \emph{value} (value occurring in \emph{column} of \code{data}),
#'   \emph{count} (number of occurrences). For \code{as.data.frame = TRUE} one
#'   data frame being the result of \code{rbind}-ing together these data frames.
#' @export
#' @examples 
#' # Some example data
#' (data <- data.frame(
#'   A = c("a1", "a2", "a1", "a1", "a2", "", "a2", NA, "a1"),
#'   B = c("b1", "b1", NA, "b2", "b2", "b1", " ", "b3", "b2")
#' ))
#' 
#' frequencyTable(data) # results in a data frame
#' 
#' frequencyTable(data, as.data.frame = FALSE) # results in a list
#'
frequencyTable <- function(
  data, columns = names(data), orderByLeastLevels = TRUE, as.data.frame = TRUE,
  useNA = c("no", "ifany", "always")[2]
)
{
  L <- frequencyTableList(data, columns = columns, useNA = useNA)
  
  if (isTRUE(orderByLeastLevels)) {
    
    L <- L[order(sapply(L, nrow))]
  }
  
  if (isTRUE(as.data.frame)) rbindAll(L) else L
}

# fullySorted ------------------------------------------------------------------

#' Sort a Data Frame by all of its Columns
#'
#' @param x data frame
#' @param decreasing passed to \code{\link[base]{order}}
#' @param ... further arguments passed to \code{\link[base]{order}}
#' @param renumber.rows if \code{TRUE} (default) the rows in the returned data
#'   frame are renumbered from 1 to the number of rows in \code{x}
#' @export
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
  
  if (renumber.rows) resetRowNames(x) else x
}

# moveToFront ------------------------------------------------------------------

#' Move elements to the start of a vector
#' 
#' @param x vector
#' @param elements elements out of \code{x} to be moved to the front
#' @return vector with \code{elements} coming first
#' @export
#' @examples 
#' moveToFront(1:10, 5)
#' moveToFront(c("a", "b", "c", "x", "y", "d"), c("x", "y"))
#'   
moveToFront <- function(x, elements)
{
  c(elements, setdiff(x, elements))
}

# noFactorDataFrame ------------------------------------------------------------

#' Create Data Frame without Factors
#'
#' Use \code{\link{data.frame}} with \code{stringsAsFactors = FALSE}
#'
#' @param \dots passed to \code{\link{data.frame}}
#' @export
#' @examples
#' # data.frame() by default converts character to factor
#' str(data.frame(id = 1:3, letter = LETTERS[1:3]))
#'
#' # noFactorDataFrame keeps character as character
#' str(noFactorDataFrame(id = 1:3, letter = LETTERS[1:3]))
#'
noFactorDataFrame <- function(...)
{
  data.frame(..., stringsAsFactors = FALSE)
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
#' @export
#' @examples 
#' L <- list(
#'   A = data.frame(x = 1:2, y = 2:3),
#'   B = data.frame(x = 1:3, y = 2:4)
#' )
#' 
#' L.unnamed <- L
#' names(L.unnamed) <- NULL
#' 
#' y1 <- rbindAll(L)
#' y2 <- rbindAll(L, nameColumn = "group")
#' y3 <- rbindAll(L.unnamed, nameColumn = "group", namesAsFactor = FALSE)
#' y4 <- rbindAll(L.unnamed, nameColumn = "group")
#' 
#' expected1 <- data.frame(
#'   x = c(L$A$x, L$B$x),
#'   y = c(L$A$y, L$B$y)
#' )
#' 
#' expected2 <- cbind(
#'   expected1,
#'   group = as.factor(c(rep("A", nrow(L$A)), rep("B", nrow(L$B)))),
#'   stringsAsFactors = FALSE
#' )
#' 
#' expected3 <- cbind(
#'   expected1,
#'   group = c(rep(1L, nrow(L$A)), rep(2L, nrow(L$B)))
#' )
#' 
#' expected4 <- expected3
#' expected4$group <- as.factor(expected4$group)
#' 
#' identical(y1, expected1) &&
#'   identical(y2, expected2) &&
#'   identical(y3, expected3) &&
#'   identical(y4, expected4)
#' 
rbindAll <- function(
  x, nameColumn = "", remove.row.names = TRUE, namesAsFactor = TRUE
)
{
  result <- do.call(rbind, x)
  
  if (nameColumn != "") {
    
    xnames <- names(x)
    
    if (is.null(xnames)) {
      xnames <- seq_along(x)
    }
    
    times <- sapply(x, FUN = function(xx) {
      if (is.null(xx)) 0 else if (is.null(dim(xx))) 1 else dim(xx)[1]
    })
    
    nameValues <- rep(xnames, times = times)
    
    if (namesAsFactor) {
      nameValues <- as.factor(nameValues)
    }
    
    if (! is.data.frame(result)) {
      result <- data.frame(result)
    }
    
    result[[nameColumn]] <- nameValues
  }
  
  if (remove.row.names) {
    row.names(result) <- NULL
  }
  
  result
}

# resetRowNames ----------------------------------------------------------------

#' Reset row names to 1:n
#' 
#' Reset the row names of a data frame x to 1:nrow(x) by setting the
#'   \code{row.names} attribute to \code{NULL}.
#' 
#' @param x data frame or matrix
#' @export
#' @examples 
#' persons <- data.frame(id = c(1, 2, 3), name = c("Peter", "Paul", "Mary"))
#' 
#' persons.ordered <- persons[order(persons$name), ]
#' 
#' # Original row names
#' persons.ordered
#' 
#' # Reset row names
#' resetRowNames(persons.ordered)
#'
resetRowNames <- function(x)
{
  if (length(dim(x)) != 2L) {
    stop(deparse(substitute(x)), " must have two dimensions", call. = FALSE)
  }

  row.names(x) <- NULL
  
  x
}

# safeRowBind ------------------------------------------------------------------

#' "safe" rbind
#' 
#' rbind two data frames even if column names differ
#' 
#' @param dataFrame1 first data frame 
#' @param dataFrame2 second data frame
#' @export
#' @examples 
#' kwb.utils::safeRowBind(
#'   data.frame(A = 1:2, B = 2:3),
#'   data.frame(B = 3:4, C = 4:5)
#' )
#'   
safeRowBind <- function(dataFrame1, dataFrame2)
{
  stopifnot(
    is.null(dataFrame1) || is.data.frame(dataFrame1),
    is.null(dataFrame2) || is.data.frame(dataFrame2)
  )
  
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
#' @return data frame resulting from "rbind"-ing all data frames in \code{x}
#' @export
#' 
safeRowBindAll <- function(x)
{
  result <- NULL

  for (element in x) {
    
    result <- safeRowBind(dataFrame1 = result, dataFrame2 = element)
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
#' @return data frame resulting from "row-binding" data frames.
#' @export
#' @examples 
#' x <- list(
#'   list(
#'     number = 1,
#'     data = data.frame(x = 1:2, y = 2:3)
#'   ),
#'   list(
#'     number = 2,
#'     data = data.frame(x = 11:12, y = 12:13)
#'   )
#' )
#'   
#' safeRowBindOfListElements(x, "data")
#'   
#' # also working if the column names of the data frames in the "data" elements
#' # differ.
#' x[[1]]$data$z = 13:14
#' safeRowBindOfListElements(x, "data")
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

# splitIntoFixSizedBlocks ------------------------------------------------------

#' Split into Blocks of Same Size
#' 
#' Split a data frame or a matrix into blocks of the same size, i.e. data frames 
#' or matrices with the same number of rows (except the last one that is shorter
#' unless the total number of rows is a multiple of the number of rows per 
#' block).
#' 
#' @param data data frame or matrix
#' @param blocksize number of rows in each block into which \code{data} is split
#' @return list of data frames (if \code{data} is a data frame) or list of
#'   matrices (if \code{data} is a matrix)
#' @export
#' 
splitIntoFixSizedBlocks <- function(data, blocksize)
{
  stopifnot(length(dim(data)) == 2)
  
  n <- nrow(data)
  
  split(data, f = rep(seq_len(n), each = blocksize, length.out = n))
}

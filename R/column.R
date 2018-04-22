# pasteColumns0 ----------------------------------------------------------------

#' Paste Columns of Data Frame Without Separator
#' 
#' @param x data frame
#' @param columns names of columns to be pasted. Default: all columns
#' @param \dots args passed to \code{\link{pasteColumns}}
#' 
#' @return vector of character with each element representing the values of the
#'   selected columns of one row, being pasted without a separator
#' 
#' @examples 
#' x <- data.frame(A = 1:3, B = 2:4)
#' pasteColumns0(x)
#' 
pasteColumns0 <- function(x, columns = names(x), ...)
{
  pasteColumns(x, columns, sep = "", ...)
}

# pasteColumns -----------------------------------------------------------------

#' Paste Columns of Data Frame With Separator
#' 
#' @param x data frame
#' @param columns names of columns to be pasted. Default: all columns
#' @param sep separator character. Default: space (" ")
#' @param \dots args passed to \code{\link{selectColumns}}, e.g. \code{do.stop}
#'   to control whether the function shall stop if not all columns exist
#'   
#' @return vector of character with each element representing the values of the
#'   selected columns of one row, being pasted with the separator character
#' 
#' @examples 
#' x <- data.frame(A = 1:3, B = 2:4)
#' pasteColumns(x, sep = ";")
#'   
pasteColumns <- function(x, columns = names(x), sep = " ", ...)
{
  if (length(columns) > 1) {
    
    args <- selectColumns(x, columns, ...)
    
    do.call(paste, c(args, sep = sep))
    
  } else {
    
    selectColumns(x, columns, ...)
  }
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
#' x1 <- NULL
#'   
#' for (i in 1:3) {
#'   
#'   x2 <- data.frame(a = 1:3, b = rnorm(3))
#'   x1 <- safeColumnBind(x1, x2)
#'   
#'   # using cbind would result in an error:
#'   # x1 <- cbind(x1, x2)
#' }
#'   
#' x1
#' 
safeColumnBind <- function(x1, x2)
{
  if (is.null(x1)) {
    
    x2
    
  } else {
    
    cbind(x1, x2)
  }
}

# posixColumnAtPosition --------------------------------------------------------

#' Indices of POSIX columns in a Data Frame
#' 
#' @param x data frame containing a date/time column
#' 
posixColumnAtPosition <- function(x)
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
#' @param x data frame in which to find a column of class "POSIXt"
#' 
firstPosixColumn <- function(x)
{
  stopifnot (is.data.frame(x))
  
  x[[posixColumnAtPosition(x)]]
}

# roundColumns -----------------------------------------------------------------

#' Round Columns to given Number of Digits
#' 
#' @param dframe data frame containing numeric columns to be rounded
#' @param columnNames names of (numeric) columns in \emph{dframe} to be rounded.
#' @param digits number of digits to be rounded to (vector of length 1 expected)
#'   or list of assignments in the form: \emph{columnName} = 
#'   \emph{numberOfDigits}. If you give a list here, then there is no need to 
#'   set the argument \emph{columnNames}
#'   
#' @return \emph{dframe} with columns given in \emph{columnNames} being rounded
#'   to \emph{digits} digits.
#' 
roundColumns <- function(dframe, columnNames = NULL, digits = NULL)
{
  # if column names are given we expect that all these columns are rounded to
  # one and the same number of digits
  if (! is.null(columnNames)) {
    
    stopifnot(length(digits) == 1)
  }
  
  if (! is.null(digits)) {
    
    if (is.null(columnNames)) {
      
      columnNames <- names(digits)
    }
    
    for (columnName in columnNames) {
      
      numberOfDigits <- if (is.list(digits)) {
        
        digits[[columnName]]
        
      } else {
        
        digits
      }
      
      dframe[[columnName]] <- round(
        dframe[[columnName]], digits = numberOfDigits
      )
    }
  }
  
  dframe
}

# selectColumns ----------------------------------------------------------------

#' Select Columns from a Data Frame
#' 
#' Select columns from a data frame. Stop with message if columns do not exist
#' 
#' @param x data frame
#' @param columns vector of column names. If \code{columns} is of length 0 or
#'   \code{NULL} (default) or \code{NA} \code{x} is returned unchanged.
#' @param drop if \code{TRUE} and if only one column is to be selected the
#'   result is a vector (one dimensional) containing the values of the selected
#'   column and not a data frame. One dimension has been \emph{dropped} then.
#'   See the \code{help("[.data.frame")}. The default is \code{TRUE} if 
#'   \code{length(columns) == 1}, else \code{FALSE}.
#' @param do.stop this flag controls whether the function stops (\code{do.stop =
#'   TRUE}) or not (\code{do.stop = FALSE}) if there are non-existing columns to
#'   be selected. If \code{do.stop = FALSE} only those columns are selected that
#'   actually exist
#'   
#' @return data frame containing the columns of \code{x} that are specified in 
#'   \code{columns} or \code{x} itself if \code{columns} is \code{NULL} or a
#'   vector containing the values of column \code{value} if \code{columns} is of
#'   length 1 and \code{drop = TRUE} (which is the default in this case).
#' 
selectColumns <- function(
  x, columns = NULL, drop = (length(columns) == 1), do.stop = TRUE
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
}

# moveColumnsToFront -----------------------------------------------------------

#' Move Columns to the Start of a Data Frame
#' 
#' move columns to the start of a data frame or matrix
#' 
#' @param x data frame
#' @param columns vector of column names
#' 
#' @return data frame or matrix with \code{columns} being the leftmost columns
#' 
#' @examples 
#' x <- data.frame(a = 1:5, b = 2:6, c = 3:7)
#'   
#' moveColumnsToFront(x, "b")
#' moveColumnsToFront(x, c("b", "a"))
#'   
moveColumnsToFront <- function(x, columns = NULL)
{
  selectColumns(x, moveToFront(names(x), columns))
}

# checkForMissingColumns -------------------------------------------------------

#' Check for Column Existence
#' 
#' Stops if data frame \emph{frm} does not contain all columns of which the 
#' names are given in \emph{reqCols}.
#' 
#' @param frm data frame
#' @param reqCols vector of names of which existence in \emph{frm} shall be
#'   checked
#' @param do.stop if TRUE, stop() is called else warning() if a column is
#'   missing
#' @param dataFrameName the name of the data frame to be shown in the error
#'   message if a column was missing
#'   
#' @return TRUE if all required columns are available, else FALSE
#'   
checkForMissingColumns <- function(
  frm, reqCols, do.stop = TRUE, dataFrameName = deparse(substitute(frm))
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
  
  isNullOrEmpty(missing)
}

# hsAddMissingCols -------------------------------------------------------------

#' Add missing Columns to a Data Frame
#' 
#' Adds missing columns to the given data frame so that the resulting data frame
#' contains all columns given in the vector \emph{colNames}. Added columns are
#' filled with NA values.
#' 
#' @param dataFrame data frame to which column names are to be appended
#' @param colNames vector containing names of columns that shall be contained in
#'   \emph{dataFrame}
#' @param fill.value value to be inserted into newly created columns. Default:
#'   \code{NA}
#'   
#' @return data frame with columns as listed in \emph{colNames}
#' 
hsAddMissingCols <- function(dataFrame, colNames, fill.value = NA)
{
  n <- nrow(dataFrame)
  
  for (col in colNames) {
    
    if (! col %in% names(dataFrame)) {
      
      dataFrame[[col]] <- rep(fill.value, n)
    }
  }
  
  dataFrame
}

# hsDelEmptyCols ---------------------------------------------------------------

#' Delete empty Columns of Data Frame
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
hsDelEmptyCols <- function(
  dataFrame, FUN = function(x) all(is.na(x)), drop = FALSE
)
{
  isEmpty <- sapply(dataFrame, FUN)
  
  dataFrame[, ! isEmpty, drop = drop]
}

# removeEmptyColumns -----------------------------------------------------------

#' Remove empty Columns from a Data Frame
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
removeEmptyColumns <- function(
  x, drop = FALSE, FUN = function(x) all(is.na(x)), dbg = TRUE
)
{
  objectName <- as.character(substitute(x))
  
  isEmpty <- sapply(x, FUN)
  
  if (any(isEmpty)) {
    
    catIf(dbg, sprintf(
      "%s: %d empty columns removed: %s\n",
      objectName,
      sum(isEmpty),
      paste(names(x)[isEmpty], collapse = ", ")
    ))
    
  } else {
    
    catIf(dbg, sprintf("%s: No empty columns.\n", objectName))
  }
  
  x[, ! isEmpty, drop = drop]
}

# removeColumns ----------------------------------------------------------------

#' Remove Columns from a Data Frame
#' 
#' @param dframe data frame,
#' @param columnsToRemove vector of column names giving the columns to remove
#' @param drop if FALSE, a data frame is returned in any case, otherwise the
#'   result may be a vector if only one column remains
#'
#' @return \emph{dframe} with columns given in \emph{columnsToRemove} being
#'   removed. User attributes of \emph{dframe} are restored.
#' 
removeColumns <- function(dframe, columnsToRemove, drop = FALSE)
{
  remainingColumns <- setdiff(names(dframe), columnsToRemove)
  
  hsRestoreAttributes(dframe[, remainingColumns, drop = drop], attributes(dframe))
}

# insertColumns ----------------------------------------------------------------

#' Insert new Column(s) into a Data Frame
#' 
#' Insert one or more new columns into a data frame before or after the given 
#' column
#' 
#' @param Data data frame
#' @param \dots named objects each of which will be a new column in the data
#'   frame. Each object must have as many elements as Data has rows.
#' @param before name of column before which to insert the new column(s)
#' @param after name of column after which to insert the new column(s)
#' @param stringsAsFactors passed on to data.frame() and cbind()
#'   
#' @return data frame \code{Data} with new columns inserted before the column
#'   named as given in \code{before} or after the column named as given in 
#'   \code{after}
#'   
#' @examples 
#' Data <- data.frame(A = 1:5, B = 2:6)
#' 
#' # Insert new columns X and Y before column "B"
#' insertColumns(Data, before = "B", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#' 
#' # This is the same as inserting new columns X and Y after column "A":
#' insertColumns(Data, after = "A", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#' 
#' # You may also insert before the first...
#' insertColumns(Data, before = "A", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#' 
#' # ... or after the last column
#' insertColumns(Data, after = "B", X = paste0("x", 1:5), Y = paste0("y", 1:5))
#'   
insertColumns <- function(
  Data, ..., before = "", after = "", 
  stringsAsFactors = default.stringsAsFactors()
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
    
    stop(
      "All new columns must be named, i.e. given in the form 'name = values'"
    )
  }
  
  # All new columns must be of equal length
  equalLength <- (sapply(newColumns, length) == nrow(Data))
  
  if (! all(equalLength)) {
    
    stop(
      "All new columns must have as many elements as Data has rows. ",
      "This is not the case for: ", commaCollapsed(columnNames[! equalLength])
    )
  }
  
  i <- which(refColumn == names(Data))
  
  n.col <- ncol(Data)
  
  partBetween <- data.frame(..., stringsAsFactors = stringsAsFactors)
  
  if (before != "") {
    
    part1 <- if (i == 1) {
      
      partBetween
      
    } else {
      
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
}

# hsRenameColumns --------------------------------------------------------------

#' Rename Columns in a Data Frame (deprecated)
#' 
#' Rename Columns in a Data Frame (deprecated, use renameColumns instead)
#' 
#' @param dframe data.frame
#' @param renames list with named elements each of which defines a column rename
#'   in the form <old-name> = <new-name>
hsRenameColumns <- function(dframe, renames)
{
  renameColumns(x = dframe, renamings = renames)
}

# renameColumns ----------------------------------------------------------------

#' Rename Columns in a Data Frame
#' 
#' Rename columns in a data frame giving tupels of original name and substitute
#' name as named elements in list "renames"
#' 
#' @param x data.frame
#' @param renamings list with named elements each of which defines a column
#'   rename in the form <old-name> = <new-name>
#' 
#' @return \emph{dframe} with columns renamed as specified in \emph{renames}
#' 
renameColumns <- function(x, renamings = NULL)
{
  if (is.null(renamings)) {
    
    return(x)
  }
  
  columns <- names(x)
  
  for (column in names(renamings)) {
    
    columns[columns == column] <- renamings[[column]]
  }
  
  structure(x, names = columns)
}

# renameAndSelect --------------------------------------------------------------

#' Rename and Select Columns of a Data Frame
#' 
#' @param data data frame
#' @param renames list defining renames in the form of "oldName" = "newName"
#'   pairs
#' @param columns (new) names of colums to be selected
#'   
renameAndSelect <- function(data, renames, columns = unlist(renames))
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
#' @param \dots column assignment(s) in the form of \code{<columnName> =
#'   <values>}
#' @param dbg if \code{TRUE} (default) the creation of new columns is reported
#'   on the screen
#'   
#' @return data frame with columns modified or appended as specified with the 
#'   \code{assignments}
#'   
#' @examples 
#'
#' # Create a data frame
#' x <- data.frame(a = 1:5)
#'
#' # Option 1: use the "$" operator
#' x1 <- x
#' x1$b <- 2:6
#' x1$c <- 3:7
#'
#' # Option 2: use setColumns
#' x2 <- setColumns(x, b = 2:6, c = 3:7)
#'
#' # The result is the same
#' identical(x1, x2)
#'
#' # but the creation of columns has been reported on the console (dbg = TRUE by
#' # default)
#'
#' ## Provide column 'b' to data frame 'x'... ok.
#' ## Provide column 'c' to data frame 'x'... ok.
#'   
setColumns <- function(.x, ..., dbg = TRUE)
{
  stopifnot(is.data.frame(.x))
  
  name.x <- deparse(substitute(.x))
  
  assignments <- list(...)
  
  if (any(is.unnamed(assignments))) {
    
    stop("All column assignments be named!", call. = FALSE)
  }
  
  for (columnName in names(assignments)) {
    
    catIf(dbg, sprintf(
      "Provide column '%s' to data frame '%s'... ", columnName, name.x
    ))
    
    .x[[columnName]] <- assignments[[columnName]]
    
    catIf(dbg, "ok.\n")
  }
  
  .x
}

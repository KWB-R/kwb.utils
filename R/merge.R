# unmerge ----------------------------------------------------------------------

#' Invert the Merging of two Data Frames
#' 
#' Split a data frame \code{z} into two data frames \code{x} and \code{y} so
#' that \code{merge(x, y)} is \code{z}.
#' 
#' @param z data frame
#' @param by vector of names of columns in \code{z} that are used to build 
#'   groups of rows of \code{z} so that within each group the values in these 
#'   columns do not change. For each group the columns being constant over all 
#'   rows are identified. Columns that are constant in each group will appear in
#'   the data frame \code{x} whereas the remaining columns will appear in the 
#'   data frame \code{y} of the returned list.
#' @return list with two elements \code{x} and \code{y} each of which are data 
#'   frames containing at least the columns given in \code{by}.
#' @export
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
#' 
unmerge <- function(z, by)
{
  stopifnot(is.data.frame(z))
  
  # Build groups of rows for each value combination in "by"-columns
  groups <- split(z, selectColumns(z, by, drop = FALSE))
  
  # Find "fix" columns in which the values do not change within any group
  fixColumnList <- excludeNULL(lapply(groups, function(x) {
    if (nrow(x) > 0) {
      y <- removeColumns(x, by)
      names(y)[sapply(y, allAreEqual)]
    }
  }))
  
  fixColumns <- Reduce(intersect, fixColumnList, init = names(z))
  
  xColumns <- c(by, fixColumns)
  
  .splitDataFrame(z, xColumns, yColumns = c(by, setdiff(names(z), xColumns)))
}

# .splitDataFrame --------------------------------------------------------------
#' @export
#' @keywords internal
#' 
.splitDataFrame <- function(z, xColumns, yColumns, check = TRUE)
{
  xdata <- unique(selectColumns(z, xColumns, drop = FALSE))
  ydata <- selectColumns(z, yColumns, drop = FALSE)
  
  if (check) {
    stopifnot(identical(ydata, unique(ydata)))
  }
  
  list(x = xdata, y = ydata)
}

# mergeAll ---------------------------------------------------------------------

#' Merge Multiple Data Frames
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
#' @return data frame being the result of merging all the data frames given in 
#'   \emph{dataFrames} by  consecutively calling \code{merge}
#' @export
#' @examples 
#' peter <- data.frame(fruit = c("apple", "pear", "banana"), kg = 1:3)
#' paul <- data.frame(fruit = c("banana", "apple", "lemon"), kg = c(10, 20, 30))
#' mary <- data.frame(fruit = c("lemon", "organger", "apple"), kg = c(22, 33, 44))
#'
#' # By default only categories that are in all data frames are returned
#' mergeAll(list(peter = peter, paul = paul, mary = mary), by = "fruit")
#'
#' # Use the arguments supported by merge to change that behaviour
#' mergeAll(list(peter = peter, paul = paul, mary = mary), by = "fruit", all = TRUE)
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
#' @export
#' 
safeMerge <- function(
  x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, ...
)
{
  checkForMissingColumns(x, by.x)
  checkForMissingColumns(y, by.y)
  
  merge(x, y, by = by, by.x = by.x, by.y = by.y, ...)
}

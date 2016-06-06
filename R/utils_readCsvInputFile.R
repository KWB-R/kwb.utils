# columnDescriptor -------------------------------------------------------------
columnDescriptor <- function # columnDescriptor
### columnDescriptor
(
  match = ".*", 
  ### pattern or fixed text to match in header line
  fixed = FALSE
  ### if TRUE, \emph{match} is taken as a fixed string to be looked for in the
  ### header line, otherwise it is interpreded as a regular expression
)
{
  list(match = match, fixed = fixed)
}

# readCsvInputFile -------------------------------------------------------------
readCsvInputFile <- function # read CSV file
### read CSV file giving column descriptions
(
  csv, 
  ### full path to CSV file
  sep,
  ### column separator
  dec,
  ### decimal character
  headerRow = 1, 
  ### number row in which the header  (containing column captions) is found
  headerPattern = "",
  ### pattern matching the header row. If \emph{headerPattern} is given
  ### \emph{headerRow} is not considered
  columnDescription = NULL,
  ### list of column descriptors. The list elements are named with the name
  ### of the list elements being the names that shall be used in the returned
  ### data frame. Each list element is a list with elements \emph{match}
  ### (pattern to be looked for in the header fields), ...
  maxRowToLookForHeader = 10,
  ### maximum number of rows to be considered when looking for the header row
  stopOnMissingColumns = TRUE,
  ### if TRUE (default) the program stops if not all columns defined in 
  ### \emph{columnDescription} are found
  encoding = "unknown",
  ### passed to readLines, "Latin-1" or "UTF-8"
  ...
  ### further arguments passed to read.table
)
{
  if (!file.exists(csv)) {
    stop("No such file: ", csv)
  }  
  
  if (headerPattern != "") {
    
    fileLines <- readLines(
      csv, n = maxRowToLookForHeader, warn = FALSE, encoding = encoding)
    
    headerRow <- grep(headerPattern, fileLines)
  }
  
  if (isNullOrEmpty(headerRow)) {
    stop(
      "I could not find the header row within the first", maxRowToLookForHeader, 
      "lines!\n  I was looking for: ", hsQuoteChr(headerPattern)
    )    
  }
  
  headerFields <- .readAndSplitRowInFile(
    csv, headerRow, sep, encoding = encoding
  )
  
  if (is.null(columnDescription)) {
    columnDescription <- .defaultColumnDescription(headerFields)
  }
  
  if (stopOnMissingColumns) {
    .stopIfNotEnoughColumns(headerFields, columnDescription, sep)    
  }
  
  newColumnDescription <- .findColumnNumbersByMatchingPatterns(
    headerFields, columnDescription
  )
  
  if (stopOnMissingColumns) {
    .stopIfNotAllColumnsFound(newColumnDescription, headerFields)    
  }
  
  .warnOnMultipleMatches(newColumnDescription, headerFields)
  
  # if there is a duplicate caption, take only the first!
  colNumbers <- as.integer(
    sapply(newColumnDescription, function(x){x[["colNumber"]][1]})
  )
  
  # remove indices of described columns that were not found
  colNames <- names(columnDescription)[!is.na(colNumbers)]
  colNumbers <- colNumbers[!is.na(colNumbers)]
  
  if (length(colNumbers) == 0) {
    warning("Not at least one of the described columns found.\n",
            .msgAvailableFields(headerFields))
    return()
  } 
  
  myData <- read.table(csv, sep=sep, dec=dec, header=FALSE, skip=headerRow, ...)
  myData <- myData[, colNumbers, drop=FALSE]
  
  names(myData) <- colNames
  
  myData
}

# .msgAvailableFields ----------------------------------------------------------
.msgAvailableFields <- function(headerFields)
{
  sprintf("\nAvailable columns:\n  %s", .numberedEnumeration(headerFields))
}

# .readAndSplitRowInFile -------------------------------------------------------
.readAndSplitRowInFile <- function(csv, rowNumber, sep, encoding, version = 2)
{
  if (version == 1 ) {
    
    fields <- read.table(
      file = csv, 
      sep = sep, 
      nrows = rowNumber, 
      fill = TRUE, 
      header = FALSE, 
      encoding = encoding
    )  
    
    fields <- tail(fields, 1)
  } 
  else {
    
    fields <- read.table(
      file = csv, 
      sep = sep, 
      skip = rowNumber - 1, 
      nrows = 1, 
      header = FALSE,
      encoding = encoding
    )
  }
  
  as.character(as.matrix(fields))
}

# .defaultColumnDescription ----------------------------------------------------
.defaultColumnDescription <- function
(
  headerFields
)
{
  columnDescription <- list()
  
  for (headerField in headerFields) {
    
    # ignore NA header fields
    if (!is.na(headerField)) {
      
      columnDescription[[.toColumnName(headerField)]] <- columnDescriptor(
        match = headerField, 
        fixed = TRUE
      )
    }
  }
  columnDescription
}

# .toColumnName ----------------------------------------------------------------
.toColumnName <- function(x)
{
  hsSubstSpecChars(x)
}

# .stopIfNotEnoughColumns ------------------------------------------------------
.stopIfNotEnoughColumns <- function(headerFields, columnDescription, sep)
{
  ncol <- length(headerFields)  
  ncolRequired <- length(columnDescription)
  
  if (ncol < ncolRequired) {
    stop(sprintf(
      paste("I found only %d of %d required column(s).",
            "Is '%s' the correct column separator?",
            "I read the following header fields:\n  %s"), 
      ncol, ncolRequired, sep, .numberedEnumeration(headerFields))
    )
  }
}

# .numberedEnumeration ---------------------------------------------------------
.numberedEnumeration <- function(x)
{
  paste0(seq_len(length(x)), ". ", hsQuoteChr(x), collapse = "\n  ")
}

# .findColumnNumbersByMatchingPatterns -----------------------------------------
.findColumnNumbersByMatchingPatterns <- function(headerFields, columnDescription)
{
  for (colName in names(columnDescription)) {
    pattern <- columnDescription[[colName]]$match
    fixed <- columnDescription[[colName]]$fixed
    columnDescription[[colName]]$colNumber <- grep(
      pattern, headerFields, fixed=ifelse(is.null(fixed), FALSE, fixed))
  }
  columnDescription
}

# .stopIfNotAllColumnsFound ----------------------------------------------------
.stopIfNotAllColumnsFound <- function(columnDescription, headerFields) 
{
  notFound <- sapply(
    columnDescription, FUN = function(x) { length(x$colNumber) == 0 }
  )
  
  if (any(notFound)) {
    msg <- "The following columns could not be found with the given patterns:\n  "
    stop(msg, 
         paste(sprintf("%s: '%s'", names(columnDescription)[notFound],
                       sapply(columnDescription[notFound], "[[", "match")),
               collapse="\n  "), 
         .msgAvailableFields(headerFields))
  }
}

# .warnOnMultipleMatches -------------------------------------------------------
.warnOnMultipleMatches <- function(columnDescription, headerFields) 
{
  ambiguous <- sapply(
    columnDescription, FUN = function(x) { length(x$colNumber) > 1 }
  )
  
  if (any(ambiguous)) {
    msg <- paste(
      "For the following patterns more than one columns were found.",
      "The first match is used in each case:\n  ",
      paste(
        sprintf(
          "'%s' matches %s", 
          sapply(columnDescription[ambiguous], "[[", "match"),
          sapply(columnDescription[ambiguous], function(x) {
            commaCollapsed(hsQuoteChr(headerFields[x$colNumber]))
          })
        )
      ),
      collapse = "\n  "
    )    
    
    warning(msg)    
  }
}

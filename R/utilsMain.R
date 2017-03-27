# getNamesOfObjectsInRDataFiles ------------------------------------------------
getNamesOfObjectsInRDataFiles <- structure(
  function # get names of objects in .RData files
  ### get names of objects in .RData files
  (
    files.rdata
    ### vector of full paths to .RData files
  )
  {
    # Create new environment into which the .RData files are to be loaded 
    # temporarily
    testenvironment <- new.env(parent = .GlobalEnv)
    
    # Prepare result list
    objectsInFiles <- list()
    
    # Loop through .RData files
    for (i in seq_len(length(files.rdata))) {
      
      cat(sprintf(
        "Loading %d/%d: %s... ", i, length(files.rdata), basename(files.rdata[i])
      ))
      load(file = files.rdata[i], envir = testenvironment)
      cat("ok. ")
      
      objectnames <- ls(envir = testenvironment)
      objectsInFiles[[i]] <- objectnames
      
      cat(length(objectnames), "objects found. ")
      
      cat("Clearing workspace... ")
      rm(list = ls(envir = testenvironment), envir = testenvironment)
      cat("ok.\n")
    }
    
    # Delete the testenvironment
    rm("testenvironment")
    
    # Return the list of object names
    structure(objectsInFiles, files.rdata = files.rdata)
  }, ex = function() {
    ## Not run
    
    ## Search for available .RData files below "searchdir"
    #searchdir <- "//poseidon/projekte$/SUW_Department/Projects/SEMA/WP/20_Braunschweig"  
    #files.rdata <- dir(searchdir, pattern = "\\\\.RData$", recursive = TRUE, full.names = TRUE)
    
    ## Get the names of the objects in the .RData files
    #objectsInFiles <- getNamesOfObjectsInRDataFiles(files.rdata = files.rdata)
    
    ## Which file contains the object "DataQ"?
    #dataQ.found <- sapply(objectsInFiles, function(x) {"DataQ" %in% x})
    
    #cat("DataQ was found in the following files:", 
    #    paste(files.rdata[dataQ.found], collapse = "\n  "))  
  })

# naToLastNonNa ----------------------------------------------------------------
naToLastNonNa <- structure(function # replace NA values with "last" non-NA value
### replace NA values in a vector with the "last" non-NA values (at the nearest
### smaller indices in each case) in the vector
(
  x,
  # vector in which NA are to be replaced with the last non-NA value (at
  # greatest of smaller indices) in the vector
  method = 2
)
{
  if (method == 1) {
    if (is.na(x[1])) {
      stop("The first element must not be NA")
    }
    
    not.na <- !is.na(x)
    
    indices.ok <- which(not.na)
    indices.na <- which(!not.na)
    
    indices.lookup <- sapply(indices.na, function(i) {
      lastElement(indices.ok[indices.ok < i])
    })
    
    replacements <- x[indices.lookup]
    
    x[indices.na] <- replacements
    
    x
  }
  else { # method = 2
    nonNaValues <- x[!is.na(x)]
    
    nonNaIndices <- which(x %in% nonNaValues)
    
    times <- diff(c(nonNaIndices, length(x) + 1))
    
    repeatedIndices <- rep(nonNaIndices, times = times)
    
    offsetIndex <- nonNaIndices[1] - 1
    
    indices <- seq_along(repeatedIndices) + offsetIndex
    
    result <- NA
    result[indices] <- x[repeatedIndices]
    
    result    
  }
}, ex = function() {
  naToLastNonNa(c(1, 2, NA, NA, 3, NA, NA, 4, NA, NA, 5)) 
  ## Result: [1] 1 2 2 2 3 3 3 4 4 4 5
  
  # You will get an error if the first element is NA!
  
  # naToLastNonNa(c(NA, 1, NA, 2))
  
  ## Error in naToLastNonNa(c(NA, 1, NA, 2)) : 
  ##   The first element must not be NA
})

# makeUnique -------------------------------------------------------------------
makeUnique <- function # adds ".1", ".2", etc. to duplicate values
### # adds ".1", ".2", etc. to duplicate values
(
  x,
  ### vector of values
  warn = TRUE
) 
{
  if (anyDuplicated(x)) {
    
    is.duplicated <- duplicated(x)
    duplicates <- unique(x[is.duplicated])
    
    if (warn) {
      warning("There are duplicate values: ", commaCollapsed(duplicates))
    }
    
    for (duplicate in duplicates) {
      
      indices <- which(x == duplicate)
      
      x[indices[-1]] <- paste(x[indices[-1]], seq_len(length(indices)-1), sep = ".")
    }
  }
  
  # Now there must not be any duplicates any more!
  stopifnot(! anyDuplicated(x))
  
  x  
  ### \emph{x} with duplicate elements being modified to "element.1",
  ### "element.2", etc.
}

# recursiveNames ---------------------------------------------------------------
recursiveNames <- function # names of all sublists of a list
### returns the names of all sublists of \emph{x} in the "$"-notation, e.g.
### list$sublist$subsublist$subsubsublist
(
  x, 
  ### R list.
  basename = ""
  ### name to be used as prefix for all names found. Default: ""
)
{
  if (!is.list(x) || is.null(names(x))) {
    return(NULL)
  }
  
  elementNames <- character()
  
  for (elementName in names(x)) {
    child <- x[[elementName]]
    if (is.list(child)) {
      newBasename <- paste(basename, elementName, sep = "$")
      elementNames <- c(
        elementNames, newBasename, recursiveNames(child, newBasename)
      )      
    }
  }
  
  elementNames
}

# getByPositiveOrNegativeIndex -------------------------------------------------
getByPositiveOrNegativeIndex <- function # getByPositiveOrNegativeIndex
### get element from vector, counting from head or tail
(
  elements, 
  ### vector of elements
  index
  ### positive or negative index(es) with absolute value between 1 and
  ### length(\emph{elements})
)
{
  n <- length(elements)
  
  if (!all(inRange(values = abs(index), min.value = 1, max.value = n))) {
    stop("There are invalid indices. The maximum allowed (absolute) value of ",
         "an index is ", n, " (number of elements).")
  }
          
  elements[toPositiveIndices(index, n = n)]
  ### element(s) out of \emph{elements} corresponding to the index(es) given in 
  ### \emph{index}
}

# toPositiveIndices ------------------------------------------------------------
toPositiveIndices <- function # toPositiveIndices
### toPositiveIndices
(
  indices, n
)
{
  negativeIndices <- which(indices < 0)
  indices[negativeIndices] <- indices[negativeIndices] + n + 1
  indices
}

# quotient ---------------------------------------------------------------------
quotient <- function # quotient
### calculate the quotient of two numbers
(
  dividend, 
  ### number to be devided
  divisor,
  ### number by which dividend is to be devided
  substitute.value = Inf,
  ### value to be returned if divisor is 0
  warn = TRUE
  ### if TRUE, a warning is given if the divisor is zero
) 
{
  resultLength <- max(length(dividend), length(divisor))
  
  result <- rep(substitute.value, times = resultLength)
  
  division <- dividend / divisor
  if (any(is.infinite(division))) {
    if (warn) {
      warning("Division by zero. Using substitute value of ", substitute.value)
    }
  }
  
  result[is.finite(division)] <- division[is.finite(division)]
  
  result
  ### quotient of dividend and divisor: dividend/divisor
}

# recycle ----------------------------------------------------------------------
recycle <- function # "recycle" vector to given length
### recycle vector to given length
(
  x, 
  ### vector to be "recycled"
  n
  ### target length
)
{
  rep(x, length.out = n)  
}

# toInches ---------------------------------------------------------------------
toInches <- function # convert cm to inches
#### convert cm to inches
(
  cm
  ### vector of numeric representing length(s) in cm
) 
{
  cm.per.inch <- 2.54
  cm / cm.per.inch
  ### vector of numeric representing length(s) in inches
}

# firstElement -----------------------------------------------------------------
firstElement <- function # first element
### Returns the first element using the function head
(
  x
  ### object
)
{
  head(x, 1)
  ### first element: x[1]
}

# lastElement ------------------------------------------------------------------
lastElement <- function # last element
### Returns the last element using the function tail
(
  x
  ### object
) 
{
  tail(x, 1)
  ### last element: x[length(x)]
}

# getFunctionName --------------------------------------------------------------
getFunctionName <- function # get the name of a function
### get the name of a function
(
  FUN
  ### R object representing a function
)
{
  deparse(quote(FUN))
}

# getFunctionValueOrDefault ----------------------------------------------------
getFunctionValueOrDefault <- function # take function value or default if NA
### take the function value or a default value if the function value is NA
(
  values, 
  ### vector of values given to \emph{FUN}
  FUN, 
  ### function to which values are passed and which offers the argument "na.rm"
  default,
  ### default value to be returned if all values are NA
  warningMessage = NA
  ### Warning message given if the default was taken
)
{
  if (all(is.na(values))) {    
    
    if (is.na(warningMessage)) {
      warningMessage <- paste(
        "All values to which", hsQuoteChr(getFunctionName(FUN)), 
        "should be applied are NA. The default is taken:", default)
    }
    
    if (warningMessage != "") {
      warning(warningMessage)      
    }
    
    functionValue <- default        
  }
  else {
    functionValue <- FUN(values, na.rm = TRUE)    
  }
  
  functionValue
}

# getOddNumbers ----------------------------------------------------------------
getOddNumbers <- function # getOddNumbers
### getOddNumbers
(
  x
) 
{
  x[isOddNumber(x)]
}
  

# getEvenNumbers ---------------------------------------------------------------
getEvenNumbers <- function # getEvenNumbers
### getEvenNumbers
(
  x
)
{
  x[isEvenNumber(x)]
}

# extendLimits -----------------------------------------------------------------
extendLimits <- function # extendLimits
### extendLimits
(
  limits, 
  ### vector of two elements as e.g. used for xlim or ylim
  left = 0.05, 
  ### percentage of limit range (\emph{absolute == FALSE}) or absolute value
  ### (\emph{absolute == TRUE}) by which the left limit is extended to the left.
  right = left,
  ### percentage of limit range (\emph{absolute == FALSE}) or absolute value 
  ### (\emph{absolute == TRUE}) by which the right limit is extended to the
  ### right.
  absolute = FALSE
  ### Default: FALSE
)
{
  if (absolute) {
    baseWidth <- 1
  }
  else {
    baseWidth <- diff(as.numeric(limits))    
  }
  newLimits <- limits + c(-left, right) * baseWidth
  hsRestoreAttributes(newLimits, attributes(limits))
}

# assignGlobally ---------------------------------------------------------------
assignGlobally <- function # assignGlobally
### assign variable in .GlobalEnv
(
  x, 
  ### name of variable
  value
  ### value of variable
)
{
  assign(x, value, envir = .GlobalEnv)  
}

# getGlobally ------------------------------------------------------------------
getGlobally <- function # getGlobally
### gat variable from .GlobalEnv
(
  x,
  ### name of variable
  default = NULL,
  ### default value to which the variable is assigned (if create.if.not.existing
  ### = TRUE) in case that it does not yet exist. Default: NULL
  create.if.not.existing = TRUE
  ### if TRUE and if the variable does not yet exist, it is created and
  ### initialised with the value given in \emph{default}. Default: TRUE  
)
{
  if (!exists(x, envir = .GlobalEnv) && create.if.not.existing) {
    assignGlobally(x, default)
  }
  
  get(x, envir=.GlobalEnv) 
}

# breakInSequence --------------------------------------------------------------
breakInSequence <- function # breakInSequence
### breakInSequence
(
  x, 
  ### vector of numeric
  expectedDiff=1
  ### expected difference between elements in x. A bigger difference is
  ### recognised as a break. Default: 1 
)
{
  stopifnot(is.numeric(x))
  
  which(diff(x) != expectedDiff)
  ### index of elements after which a break occurs or integer(0) if no break
  ### occurs at all
}

# warnIfEmpty ------------------------------------------------------------------
warnIfEmpty <- function # warnIfEmpty
### Gives a warning if the object is NULL or empty and returns the object
(
  x
  ### object to be tested for NULL or being empty (vector of length 0 or 
  ### data frame with no rows)
) 
{
  if (isNullOrEmpty(x)) {
    warning("\n\n*** The object is empty!\n")
  }
  
  x
}

# hsRestoreAttributes ----------------------------------------------------------
hsRestoreAttributes <- function # Restores object attributes
### Restores given attributes that are not object attributes any more
(
  x, 
  ### object
  attribs
  ### former attributes of x (as retrieved by attributes(x)) to be restored
)
{
  for (attrib in setdiff(names(attribs), names(attributes(x)))) {
    attr(x, attrib) <- attribs[[attrib]]
  }
  x
}

# removeAttributes -------------------------------------------------------------
removeAttributes <- function # Returns object without attributes
### Returns object without attributes
(
  x
  ### object
)
{
  attributes(x) <- NULL

  x
  ### \emph{x}, but with its attributes removed
}

# hsMatrixToListForm -----------------------------------------------------------
hsMatrixToListForm <- function # convert "matrix form" to "list form"
### converts a data frame in "matrix form" to a data frame in "list form"
##seealso<< stats::reshape
(
  df, 
  ### data frame
  keyFields, 
  ### names of key fields (e.g. date/time)
  parFields = setdiff(names(df), keyFields), 
  ### names of fields representing differen parameters. Default: column names
  ### that are not in \emph{keyFields}
  colNamePar = "parName", 
  ### name of column in result data frame that will contain the parameter names
  colNameVal = "parVal",
  ### name of column in result data frame that will contain the parameter values
  stringsAsFactors = FALSE
  ### if TRUE, columns of type character in the result data frame are converted
  ### to factors. Parameter is passed to cbind, rbind.
) 
{  
  ## set options "stringsAsFactors" and reset on exit
  saf <- options()$stringsAsFactors
  options(stringsAsFactors = stringsAsFactors) 
  on.exit(options(stringsAsFactors = saf))
  
  ## prepare output data frame
  dfo <- data.frame()
  
  ## loop through column names 
  for (fname in parFields) {
    
    ## sub-data frame    
    sdf <- cbind(df[, keyFields, drop = FALSE], fname, df[, fname])
    
    ## bind sub-data frame to output data frame. 
    dfo <- rbind(dfo, sdf) 
  }
  
  names(dfo) <- c(keyFields, colNamePar, colNameVal)
  dfo
  ### data frame in "list form"
}

# hsSafeName -------------------------------------------------------------------
hsSafeName <- structure( 
  function # Non-existing desired name
  ### Returns a name that is not yet contained in a vector \emph{myNames} 
  ### of existing names. 
  #@2011-12-23: created
  (
    myName, ##<< desired name. 
    myNames ##<< vector of existing names.
  ) {
    ptrn <- "_(\\d+)$" ## numeric digits at the end, separated by underscore 
    
    ## While the name is in the list of existing names
    while (myName %in% myNames) {
      
      ## If the name ends with a version number...
      if (grepl(ptrn, myName)) {      
        
        ## Get the version number
        pos <- regexpr(ptrn, myName) + 1 ## Position of the "version" number
        vnr <- as.integer(substr(myName, pos, nchar(myName)))
        
        ## Remove the version number
        myName <- sub(ptrn, "", myName)
        
        ## Append the incremented version number
        myName <- sprintf("%s_%d", myName, vnr + 1)
      }
      ## If the name does not end with a number, append "_1"
      else {
        myName <- sprintf("%s_1", myName)
      }
    }
    myName
    ### If \emph{myName} is not contained in \emph{myNames} it is returned. 
    ### Otherwise \emph{myName} is modified to \emph{myName}_01, \emph{myName}_02, 
    ### ... until a non-existing name is found that is then returned.
  },
  ex = function() {
    existing <- c("a", "b")
    myName <- hsSafeName("c", existing) 
    myName                               # "c"
    myName <- hsSafeName("a", existing) 
    myName                               # "a_1"
    hsSafeName("a", c(existing, myName)) # "a_2"
  })


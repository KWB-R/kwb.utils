# randomValuesWithSum ----------------------------------------------------------

#' vector of random integer values of given sum
#' 
#' vector of random integer values of given sum
#' 
#' @param n number of values
#' @param sumOfValues sum of values in the result vector
#' @param names names of elements in the result vector. Default: \code{seq_len(n)}
#' 
#' @return named vector of integer values with \code{sum(values) == sumOfValues}
#' 
randomValuesWithSum <- function # vector of random integer values of given sum
### vector of random integer values of given sum
(
  n,
  ### number of values
  sumOfValues,
  ### sum of values in the result vector
  names = seq_len(n)
  ### names of elements in the result vector. Default: \code{seq_len(n)}
)
{
  breaks <- sort(sample(sumOfValues, n - 1))

  values <- diff(c(0, breaks, sumOfValues))

  valueSum <- sum(values)

  if (valueSum != sumOfValues) {
    stop("Bug in randomValuesWithSum(): The sum of generated values is not ",
         sumOfValues, " as requested but ", valueSum, "!")
  }

  structure(values, names = names)

  ### named vector of integer values with \code{sum(values) == sumOfValues}
}

# callWithStringsAsFactors -----------------------------------------------------

#' Call a function with option "stringsAsFactors" set temporarily
#' 
#' Set the option "stringsAsFactors", run a function and reset the option.
#' 
#' @param stringsAsFactors TRUE or FALSE. Before calling \code{FUN} the option "stringsAsFactors" is
#'   set to the value given here. After the function call the option is reset
#'   to what it was before.
#' @param FUN function to be called
#' @param \dots arguments passed to \code{FUN}
#' 
#' @return This function returns what \code{FUN} returns when called with the
#'   arguments given in \code{...}
#' 
#' @examples 
#'   
#'   option.bak <- getOption("stringsAsFactors")
#'   
#'   d1 <- callWithStringsAsFactors(
#'     TRUE,
#'     rbind,
#'     data.frame(id = 1, name = "Peter"),
#'     data.frame(id = 2, name = "Paul"),
#'     data.frame(id = 3, name = "Mary")
#'   )
#'   
#'   d2 <- callWithStringsAsFactors(
#'     FALSE,
#'     rbind,
#'     data.frame(id = 1, name = "Peter"),
#'     data.frame(id = 2, name = "Paul"),
#'     data.frame(id = 3, name = "Mary")
#'   )
#'   
#'   str(d1)
#'   str(d2)
#'   
#'   # The option "stringsAsFactors" has not changed!
#'   stopifnot(option.bak == getOption("stringsAsFactors"))
#'   
#' 
callWithStringsAsFactors <- structure(
  function # Call a function with option "stringsAsFactors" set temporarily
  ### Set the option "stringsAsFactors", run a function and reset the option.
(
  stringsAsFactors,
  ### TRUE or FALSE. Before calling \code{FUN} the option "stringsAsFactors" is
  ### set to the value given here. After the function call the option is reset
  ### to what it was before.
  FUN,
  ### function to be called
  ...
  ### arguments passed to \code{FUN}
)
{
  if (! identical(TRUE, stringsAsFactors) &&
      ! identical(FALSE, stringsAsFactors)) {
    stop("stringsAsFactors must be TRUE or FALSE")
  }

  # Save current value of option "stringsAsFactors"
  stringsAsFactors.bak <- getOption("stringsAsFactors")

  # Set option "stringsAsFactors"
  options(stringsAsFactors = stringsAsFactors)

  # Call the given function
  result <- FUN(...)

  # Reset option "stringsAsFactors"
  options(stringsAsFactors = stringsAsFactors.bak)

  # Return the result
  result

  ### This function returns what \code{FUN} returns when called with the
  ### arguments given in \code{...}
}, ex = function() {

  option.bak <- getOption("stringsAsFactors")

  d1 <- callWithStringsAsFactors(
    TRUE,
    rbind,
    data.frame(id = 1, name = "Peter"),
    data.frame(id = 2, name = "Paul"),
    data.frame(id = 3, name = "Mary")
  )

  d2 <- callWithStringsAsFactors(
    FALSE,
    rbind,
    data.frame(id = 1, name = "Peter"),
    data.frame(id = 2, name = "Paul"),
    data.frame(id = 3, name = "Mary")
  )

  str(d1)
  str(d2)

  # The option "stringsAsFactors" has not changed!
  stopifnot(option.bak == getOption("stringsAsFactors"))
})

# parallelNonNA ----------------------------------------------------------------

#' merge two vectors selecting non-NA values
#' 
#' two vectors \emph{a} and \emph{b} of same length are merged in such a way
#'   that at index i the result vector contains
#'   (1) a[i] if a[i] is not NA and b[i] is NA or b[i] == a[i],
#'   (2) b[i] if b[i] is not NA and a[i] is NA or a[i] == b[i],
#'   (3) "" if a[i] is not NA and b[i] is not NA and a[i] != b[i] or if both
#'   a[i] and b[i] are NA
#' 
#' @param a vector 1
#' @param b vector 2
#' 
#' @return character vector of same length as \emph{a} and \emph{b}
#' 
#' @examples 
#'   parallelNonNA(c(1, NA, 3), c(NA, 2, NA))  # "1" "2" "3"
#'   
#'   parallelNonNA(c(1, NA, NA), c(NA, 2, NA)) # "1" "2" ""
#'   
#'   ## A warning is given if (non-NA) values at the same index differ
#'   y <- parallelNonNA(c(1, 2, 3), c(1, 2, 4))
#'   
#'   y # "1" "2" ""
#'   
#'   ## attribute "invalid" contains the index and values of the differing values
#'   attr(y, "invalid")
#'   
#' 
parallelNonNA <- structure(
  function # merge two vectors selecting non-NA values
### two vectors \emph{a} and \emph{b} of same length are merged in such a way
### that at index i the result vector contains
### (1) a[i] if a[i] is not NA and b[i] is NA or b[i] == a[i],
### (2) b[i] if b[i] is not NA and a[i] is NA or a[i] == b[i],
### (3) "" if a[i] is not NA and b[i] is not NA and a[i] != b[i] or if both
### a[i] and b[i] are NA
(
  a,
  ### vector 1
  b
  ### vector 2
)
{
  stopifnot(length(a) == length(b))

  result <- character(length(a))

  selected <- !is.na(a) & is.na(b)
  result[selected] <- a[selected]

  selected <- is.na(a) & !is.na(b)
  result[selected] <- b[selected]

  bothNotNA <- !is.na(a) & !is.na(b)

  selected <- bothNotNA & (a == b)
  result[selected] <- a[selected]

  selected <- bothNotNA & (a != b)

  if (any(selected)) {

    attr(result, "invalid") <- data.frame(
      index = which(selected),
      a = a[selected],
      b = b[selected]
    )

    warning("There are differences in parallel non-NA values ",
            "(returned in attribute 'invalid')")
  }

  result
  ### character vector of same length as \emph{a} and \emph{b}
}, ex = function() {
  parallelNonNA(c(1, NA, 3), c(NA, 2, NA))  # "1" "2" "3"

  parallelNonNA(c(1, NA, NA), c(NA, 2, NA)) # "1" "2" ""

  ## A warning is given if (non-NA) values at the same index differ
  y <- parallelNonNA(c(1, 2, 3), c(1, 2, 4))

  y # "1" "2" ""

  ## attribute "invalid" contains the index and values of the differing values
  attr(y, "invalid")
})

# naToLastNonNa ----------------------------------------------------------------

#' Replace NA With "Last" non-NA
#' 
#' replace NA values in a vector with the "last" non-NA values (at the nearest
#'   smaller indices in each case) in the vector
#' 
#' @param x vector in which NA are to be replaced with the last non-NA value 
#'   (at greatest of smaller indices) in the vector
#' @param method integer (1 or 2) distinguishing two different methods
#' 
#' @examples 
#'   naToLastNonNa(c(1, 2, NA, NA, 3, NA, NA, 4, NA, NA, 5))
#'   ## Result: [1] 1 2 2 2 3 3 3 4 4 4 5
#'   
#'   # You will get an error if the first element is NA!
#'   
#'   # naToLastNonNa(c(NA, 1, NA, 2))
#'   
#'   ## Error in naToLastNonNa(c(NA, 1, NA, 2)) :
#'   ##   The first element must not be NA
#'   
naToLastNonNa <- function(x, method = 2)
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
}

# makeUnique -------------------------------------------------------------------

#' Make Duplicated Character Strings Unique
#'  
#' adds ".1", ".2", etc. to duplicate values
#' 
#' @param x vector of character strings
#' @param warn if \code{TRUE} (default) a warning showing the duplicated values is 
#'   given
#' 
#' @return \code{x} with duplicate elements being modified to "element.1",
#'   "element.2", etc.
#' 
makeUnique <- function(x, warn = TRUE)
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
}

# recursiveNames ---------------------------------------------------------------

#' names of all sublists of a list
#' 
#' returns the names of all sublists of \emph{x} in the "$"-notation, e.g.
#'   list$sublist$subsublist$subsubsublist
#' 
#' @param x R list.
#' @param basename name to be used as prefix for all names found. Default: ""
#' 
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

# quotient ---------------------------------------------------------------------

#' quotient
#' 
#' calculate the quotient of two numbers
#' 
#' @param dividend number to be devided
#' @param divisor number by which dividend is to be devided
#' @param substitute.value value to be returned if divisor is 0
#' @param warn if TRUE, a warning is given if the divisor is zero
#' 
#' @return quotient of dividend and divisor: dividend/divisor
#' 
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

# getFunctionName --------------------------------------------------------------

#' get the name of a function
#' 
#' get the name of a function
#' 
#' @param FUN R object representing a function
#' 
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

#' take function value or default if NA
#' 
#' take the function value or a default value if the function value is NA
#' 
#' @param values vector of values given to \emph{FUN}
#' @param FUN function to which values are passed and which offers the argument "na.rm"
#' @param default default value to be returned if all values are NA
#' @param warningMessage Warning message given if the default was taken
#' 
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

#' getOddNumbers
#' 
#' getOddNumbers
#' 
#' @param x vector of integer
#' 
getOddNumbers <- function(x)
{
  x[isOddNumber(x)]
}

# getEvenNumbers ---------------------------------------------------------------

#' getEvenNumbers
#' 
#' getEvenNumbers
#' 
#' @param x vector of integer
getEvenNumbers <- function(x)
{
  x[isEvenNumber(x)]
}

# extendLimits -----------------------------------------------------------------

#' extendLimits
#' 
#' extendLimits
#' 
#' @param limits vector of two elements as e.g. used for xlim or ylim
#' @param left percentage of limit range (\emph{absolute == FALSE}) or absolute value
#'   (\emph{absolute == TRUE}) by which the left limit is extended to the left.
#' @param right percentage of limit range (\emph{absolute == FALSE}) or absolute value
#'   (\emph{absolute == TRUE}) by which the right limit is extended to the
#'   right.
#' @param absolute Default: FALSE
#' 
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

# assignAll --------------------------------------------------------------------

#' call assign for each list element
#' 
#' Provide all assignments of a list as objects in the environment
#' 
#' @param x list of \code{key = value} assignments
#' @param pos passed to \code{assign},
#' @param \dots further arguments passed to \code{assign}
#' 
assignAll <- function # call assign for each list element
### Provide all assignments of a list as objects in the environment
(
  x,
  ### list of \code{key = value} assignments
  pos = 1,
  ### passed to \code{assign},
  ...
  ### further arguments passed to \code{assign}
)
{
  stopifnot(is.list(x))

  for (key in names(x)) {
    assign(key, x[[key]], pos = pos, ...)
  }
}

# assignPackageObjects ---------------------------------------------------------
#' Assign all Package Objects to the Global Environment
#' 
#' @param package package name
#' @examples 
#' assignPackageObjects("kwb.utils")
assignPackageObjects <- function(package)
{
  for (name in ls(getNamespace(package))) {
    object <- get(name, envir = asNamespace(package), inherits = FALSE)
    assignGlobally(name, object)  
  }
}

# assignGlobally ---------------------------------------------------------------

#' assignGlobally
#' 
#' assign variable in .GlobalEnv
#' 
#' @param x name of variable
#' @param value value of variable
#' 
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

#' getGlobally
#' 
#' gat variable from .GlobalEnv
#' 
#' @param x name of variable
#' @param default default value to which the variable is assigned (if create.if.not.existing
#'   = TRUE) in case that it does not yet exist. Default: NULL
#' @param create.if.not.existing if TRUE and if the variable does not yet exist, it is created and
#'   initialised with the value given in \emph{default}. Default: TRUE
#' 
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

#' breakInSequence
#' 
#' breakInSequence
#' 
#' @param x vector of numeric
#' @param expectedDiff expected difference between elements in x. A bigger difference is
#'   recognised as a break. Default: 1
#' 
#' @return index of elements after which a break occurs or integer(0) if no break
#'   occurs at all
#' 
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

#' warnIfEmpty
#' 
#' Gives a warning if the object is NULL or empty and returns the object
#' 
#' @param x object to be tested for NULL or being empty (vector of length 0 or
#'   data frame with no rows)
#' 
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

# hsMatrixToListForm -----------------------------------------------------------

#' convert "matrix form" to "list form"
#' 
#' converts a data frame in "matrix form" to a data frame in "list form"
#' 
#' @param df data frame
#' @param keyFields names of key fields (e.g. date/time)
#' @param parFields names of fields representing differen parameters. Default: column names
#'   that are not in \emph{keyFields}
#' @param colNamePar name of column in result data frame that will contain the parameter names
#' @param colNameVal name of column in result data frame that will contain the parameter values
#' @param stringsAsFactors if TRUE, columns of type character in the result data frame are converted
#'   to factors. Parameter is passed to cbind, rbind.
#' 
#' @return data frame in "list form"
#' 
#' @seealso \code{\link[stats]{reshape}}
#' 
hsMatrixToListForm <- function # convert "matrix form" to "list form"
### converts a data frame in "matrix form" to a data frame in "list form"
##seealso<< \code{\link[stats]{reshape}}
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
  options.now <- options(stringsAsFactors = stringsAsFactors)
  on.exit(options(options.now))

  ## loop through column names
  result <- rbindAll(lapply(parFields, function(field) {
    cbind(df[, keyFields, drop = FALSE], field, df[, field])
  }))

  structure(result, names = c(keyFields, colNamePar, colNameVal))
  ### data frame in "list form"
}

# hsSafeName -------------------------------------------------------------------

#' Non-existing desired name
#' 
#' Returns a name that is not yet contained in a vector \emph{myNames}
#'   of existing names.
#' 
#' @param myName desired name.
#' @param myNames vector of existing names.
#' 
#' @return If \emph{myName} is not contained in \emph{myNames} it is returned.
#'   Otherwise \emph{myName} is modified to \emph{myName}_01, \emph{myName}_02,
#'   ... until a non-existing name is found that is then returned.
#' 
#' @examples 
#'   existing <- c("a", "b")
#'   myName <- hsSafeName("c", existing)
#'   myName                               # "c"
#'   myName <- hsSafeName("a", existing)
#'   myName                               # "a_1"
#'   hsSafeName("a", c(existing, myName)) # "a_2"
#'   
#' 
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

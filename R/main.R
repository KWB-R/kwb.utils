# randomValuesWithSum ----------------------------------------------------------

#' Vector of random Integer Values of given Sum
#' 
#' @param n number of values
#' @param sumOfValues sum of values in the result vector
#' @param names names of elements in the result vector. Default:
#'   \code{seq_len(n)}
#' @return named vector of integer values with \code{sum(values) == sumOfValues}
#' @export
#' 
randomValuesWithSum <- function(n, sumOfValues, names = seq_len(n))
{
  breaks <- sort(sample(sumOfValues, n - 1))

  values <- diff(c(0, breaks, sumOfValues))

  valueSum <- sum(values)

  if (valueSum != sumOfValues) {
    stop("Bug in randomValuesWithSum(): The sum of generated values is not ",
         sumOfValues, " as requested but ", valueSum, "!")
  }

  structure(values, names = names)
}

# callWithStringsAsFactors -----------------------------------------------------

#' Call a Function with Option "stringsAsFactors" set temporarily
#' 
#' Set the option "stringsAsFactors", run a function and reset the option.
#' 
#' @param stringsAsFactors TRUE or FALSE. Before calling \code{FUN} the option
#'   "stringsAsFactors" is set to the value given here. After the function call
#'   the option is reset to what it was before.
#' @param FUN function to be called
#' @param \dots arguments passed to \code{FUN}
#' @return This function returns what \code{FUN} returns when called with the
#'   arguments given in \code{...}
#' @export
#' @examples 
#' option.bak <- getOption("stringsAsFactors")
#'   
#' d1 <- callWithStringsAsFactors(
#'   TRUE,
#'   rbind,
#'   data.frame(id = 1, name = "Peter"),
#'   data.frame(id = 2, name = "Paul"),
#'   data.frame(id = 3, name = "Mary")
#' )
#'   
#' d2 <- callWithStringsAsFactors(
#'   FALSE,
#'   rbind,
#'   data.frame(id = 1, name = "Peter"),
#'   data.frame(id = 2, name = "Paul"),
#'   data.frame(id = 3, name = "Mary")
#' )
#'   
#' str(d1)
#' str(d2)
#'   
#' # The option "stringsAsFactors" has not changed!
#' stopifnot(option.bak == getOption("stringsAsFactors"))
#'   
callWithStringsAsFactors <- function(stringsAsFactors, FUN, ...)
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
}

# parallelNonNA ----------------------------------------------------------------

#' Merge two Vectors selecting non-NA Values
#' 
#' two vectors \emph{a} and \emph{b} of same length are merged in such a way 
#' that at index i the result vector contains (1) a[i] if a[i] is not NA and
#' b[i] is NA or b[i] == a[i], (2) b[i] if b[i] is not NA and a[i] is NA or a[i]
#' == b[i], (3) "" if a[i] is not NA and b[i] is not NA and a[i] != b[i] or if
#' both a[i] and b[i] are NA
#' 
#' @param a vector 1
#' @param b vector 2
#' @return character vector of same length as \emph{a} and \emph{b}
#' @export
#' @examples 
#' parallelNonNA(c(1, NA, 3), c(NA, 2, NA))  # "1" "2" "3"
#'   
#' parallelNonNA(c(1, NA, NA), c(NA, 2, NA)) # "1" "2" ""
#'   
#' ## A warning is given if (non-NA) values at the same index differ
#' y <- parallelNonNA(c(1, 2, 3), c(1, 2, 4))
#'   
#' y # "1" "2" ""
#'   
#' ## attribute "invalid" contains the index and values of the differing values
#' attr(y, "invalid")
#'   
parallelNonNA <- function(a, b)
{
  stopifnot(length(a) == length(b))

  # Create result vector of NA, as long as "a" and of same class as "a"
  result <- `class<-`(rep(NA, length(a)), class(a))

  onlyA <- !is.na(a) & is.na(b)
  onlyB <- is.na(a) & !is.na(b)
  
  aAndB <- !is.na(a) & !is.na(b)
  aAndBAndEqual <- aAndB & (a == b)
  aAndBAndDiffer <- aAndB & (a != b)
  
  result[onlyA] <- a[onlyA]
  result[onlyB] <- b[onlyB]
  result[aAndBAndEqual] <- a[aAndBAndEqual]

  if (any(aAndBAndDiffer)) {

    attr(result, "invalid") <- data.frame(
      index = which(aAndBAndDiffer),
      a = a[aAndBAndDiffer],
      b = b[aAndBAndDiffer]
    )

    warning("There are differences in parallel non-NA values ",
            "(returned in attribute 'invalid')")
  }

  result
}

# naToLastNonNa ----------------------------------------------------------------

#' Replace NA With "Last" non-NA
#' 
#' replace NA values in a vector with the "last" non-NA values (at the nearest
#'   smaller indices in each case) in the vector
#' 
#' @param x vector in which NA are to be replaced with the last non-NA value 
#'   (at greatest of smaller indices) in the vector
#' @param method integer (1 or 2) distinguishing two different methods
#' @export
#' @examples 
#' naToLastNonNa(c(1, 2, NA, NA, 3, NA, NA, 4, NA, NA, 5))
#' ## Result: [1] 1 2 2 2 3 3 3 4 4 4 5
#'   
#' # You will get an error if method = 1 and the first element is NA!
#'   
#' # naToLastNonNa(c(NA, 1, NA, 2), method = 1)
#'   
#' ## Error in naToLastNonNa(c(NA, 1, NA, 2)) :
#' ##   The first element must not be NA
#'   
naToLastNonNa <- function(x, method = 2)
{
  is_na <- is.na(x)
  
  if (method == 1 && is_na[1]) {
    
    stop("The first element must not be NA")
  }
  
  indices_ok <- which(! is_na)
  
  if (method == 1) {
    
    indices_to <- which(is_na)
    
    indices_from <- sapply(indices_to, function(index_ok) {
      lastElement(indices_ok[indices_ok < index_ok])
    })

  } else { # method = 2

    indices_from <- rep(indices_ok, times = diff(c(indices_ok, length(x) + 1)))
    
    indices_to <- seq_along(indices_from) + indices_ok[1] - 1
  }
  
  x[indices_to] <- x[indices_from]

  x
}

# makeUnique -------------------------------------------------------------------

#' Make Duplicated Character Strings Unique
#'  
#' adds ".1", ".2", etc. to duplicate values
#' 
#' @param x vector of character strings
#' @param warn if \code{TRUE} (default) a warning showing the duplicated values is 
#'   given
#' @param sep separator between name and suffix number. Default: "."
#' @param simple if \code{TRUE} all elements with identical name (e.g. "a") are 
#'   numbered (e.g. "a.1", "a.2", "a.3"), otherwise the first element is kept
#'   unchanged and all but the first one are numbered (e.g. "a", "a.1", "a.2").
#'   The default is \code{FALSE}.
#' @return \code{x} with duplicate elements being modified to "element.1",
#'   "element.2", etc.
#' @export
#' 
makeUnique <- function(x, warn = TRUE, sep = ".", simple = FALSE)
{
  if (anyDuplicated(x)) {

    is.duplicated <- duplicated(x)
    
    duplicates <- unique(x[is.duplicated])

    if (warn) {
      warning("There are duplicate values: ", stringList(duplicates))
    }

    for (duplicate in duplicates) {

      indices <- which(x == duplicate)

      if (! simple) {
        indices <- indices[-1L]
      }

      x[indices] <- paste(x[indices], seq_along(indices), sep = sep)
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
#' @export
#' 
recursiveNames <- function(x, basename = "")
{
  if (! is.list(x) || is.null(names(x))) {
    
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

#' Quotient
#' 
#' Calculate the quotient of two numbers
#' 
#' @param dividend number to be devided
#' @param divisor number by which dividend is to be devided
#' @param substitute.value value to be returned if divisor is 0
#' @param warn if TRUE, a warning is given if the divisor is zero
#' @return quotient of dividend and divisor: dividend/divisor
#' @export
#' 
quotient <- function(dividend, divisor, substitute.value = Inf, warn = TRUE)
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
}

# getOddNumbers ----------------------------------------------------------------

#' Get odd Numbers out of a Vector of Integers
#' 
#' @param x vector of integer
#' @export
#' 
getOddNumbers <- function(x)
{
  x[isOddNumber(x)]
}

# getEvenNumbers ---------------------------------------------------------------

#' Get even Numbers out of a Vector of Integers
#' 
#' @param x vector of integer
#' @export
#' 
getEvenNumbers <- function(x)
{
  x[isEvenNumber(x)]
}

# extendLimits -----------------------------------------------------------------

#' Extend the Limits of a Range Vector
#' 
#' @param limits vector of two elements as e.g. used for xlim or ylim
#' @param left percentage of limit range (\emph{absolute == FALSE}) or absolute value
#'   (\emph{absolute == TRUE}) by which the left limit is extended to the left.
#' @param right percentage of limit range (\emph{absolute == FALSE}) or absolute value
#'   (\emph{absolute == TRUE}) by which the right limit is extended to the
#'   right.
#' @param absolute Default: FALSE
#' @export
#' 
extendLimits <- function(limits, left = 0.05, right = left, absolute = FALSE)
{
  baseWidth <- if (absolute) 1 else diff(as.numeric(limits))

  newLimits <- limits + c(-left, right) * baseWidth
  
  hsRestoreAttributes(newLimits, attributes(limits))
}

# assignAll --------------------------------------------------------------------

#' Call assign for each List Element
#' 
#' Provide all assignments of a list as objects in the environment
#' 
#' @param x list of \code{key = value} assignments
#' @param pos passed to \code{assign},
#' @param \dots further arguments passed to \code{assign}
#' @export
#' 
assignAll <- function(x, pos = 1, ...)
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
#' @export
#' @examples 
#' assignPackageObjects("kwb.utils")
#' 
assignPackageObjects <- function(package)
{
  for (name in ls(getNamespace(package))) {
    object <- get(name, envir = asNamespace(package), inherits = FALSE)
    assign(name, object, envir = .GlobalEnv)
  }
}

# assignGlobally ---------------------------------------------------------------

#' assignGlobally
#' 
#' assign variable in .GlobalEnv
#' 
#' @param x name of variable
#' @param value value of variable
#' @export
#' 
assignGlobally <- function(x, value)
{
  warningDeprecated(
    old_name = "assignGlobally()", 
    new_name = "'assign(x, value, envir = .GlobalEnv)'", 
    parentheses = FALSE
  )
  
  assign(x, value, envir = .GlobalEnv)
}

# getGlobally ------------------------------------------------------------------

#' getGlobally
#' 
#' gat variable from .GlobalEnv
#' 
#' @param x name of variable
#' @param default default value to which the variable is assigned (if
#'   create.if.not.existing = TRUE) in case that it does not yet exist. Default:
#'   NULL
#' @param create.if.not.existing if TRUE and if the variable does not yet exist,
#'   it is created and initialised with the value given in \code{default}.
#'   Default: TRUE
#' @export
#' 
getGlobally <- function(x, default = NULL, create.if.not.existing = TRUE)
{
  if (!exists(x, envir = .GlobalEnv) && create.if.not.existing) {
    assign(x, default, envir = .GlobalEnv)
  }

  get(x, envir = .GlobalEnv)
}

# breakInSequence --------------------------------------------------------------

#' Find "Breaks" in a Sequence of Numbers
#' 
#' @param x vector of numeric
#' @param expectedDiff expected difference between elements in x. A bigger
#'   difference is recognised as a break. Default: 1
#' @return index of elements after which a break occurs or integer(0) if no
#'   break occurs at all
#' @export
#' 
breakInSequence <- function(x, expectedDiff = 1)
{
  stopifnot(is.numeric(x))

  which(diff(x) != expectedDiff)
}

# warnIfEmpty ------------------------------------------------------------------

#' warnIfEmpty
#' 
#' Gives a warning if the object is NULL or empty and returns the object
#' 
#' @param x object to be tested for NULL or being empty (vector of length 0 or
#'   data frame with no rows)
#' @export
#' 
warnIfEmpty <- function(x)
{
  if (isNullOrEmpty(x)) {
    
    warning("\n\n*** The object is empty!\n")
  }

  x
}

# hsMatrixToListForm -----------------------------------------------------------

#' Convert "Matrix Form" (wide format) to "List Form" (long format)
#' 
#' Converts a data frame in "matrix form" to a data frame in "list form"
#' 
#' @param df data frame
#' @param keyFields names of key fields (e.g. date/time)
#' @param parFields names of fields representing differen parameters. Default:
#'   column names that are not in \emph{keyFields}
#' @param colNamePar name of column in result data frame that will contain the
#'   parameter names
#' @param colNameVal name of column in result data frame that will contain the
#'   parameter values
#' @param stringsAsFactors if TRUE, columns of type character in the result data
#'   frame are converted to factors. Parameter is passed to cbind, rbind.
#' @return data frame in "list form" (long format)
#' @export
#' @seealso \code{\link[stats]{reshape}}
#' 
hsMatrixToListForm <- function(
  df, keyFields, parFields = setdiff(names(df), keyFields), 
  colNamePar = "parName", colNameVal = "parVal", stringsAsFactors = FALSE
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
}

# hsSafeName -------------------------------------------------------------------

#' Non-existing desired name
#' 
#' Returns a name that is not yet contained in a vector \emph{myNames}
#'   of existing names.
#' 
#' @param myName desired name.
#' @param myNames vector of existing names.
#' @return If \emph{myName} is not contained in \emph{myNames} it is returned.
#'   Otherwise \emph{myName} is modified to \emph{myName}_01, \emph{myName}_02,
#'   ... until a non-existing name is found that is then returned.
#' @export
#' @examples 
#' existing <- c("a", "b")
#' myName <- hsSafeName("c", existing)
#' myName                               # "c"
#' myName <- hsSafeName("a", existing)
#' myName                               # "a_1"
#' hsSafeName("a", c(existing, myName)) # "a_2"
#'   
hsSafeName <- function(myName, myNames)
{
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
}

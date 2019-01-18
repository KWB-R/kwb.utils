# intToNumeralSystem -----------------------------------------------------------

#' Convert Integers to Numeral System
#' 
#' @param x vector of integers
#' @param base base of the numeral system
#' @return matrix with as many rows as there are elements in \code{x} and as 
#'   many columns as digits that are required to represent the integers in 
#'   \code{x} in the numeral system in base \code{base}. The elements of 
#'   \code{x} appear as row names whereas the powers of \code{base} appear as
#'   column names.
#' @examples
#' intToNumeralSystem(1:16, base = 2) # binary system
#' intToNumeralSystem(1:16, base = 10) # decimal system
#' intToNumeralSystem(1:16, base = 8) # octal system
intToNumeralSystem <- function(x, base) 
{
  stopifnot(is.integer(x))
  
  maxi <- max(x)
  
  ref <- 1
  
  max_power <- 0
  
  while (maxi >= ref) {
    
    ref <- ref * base
    max_power <- max_power + 1
  }
  
  ref <- ref / base
  max_power <- max_power - 1
  
  result <- createMatrix(
    rowNames = as.character(x), 
    colNames = as.character(rev(base^(0:max_power))), 
    value = 0L
  )
  
  power <- max_power
  
  while (power > 0) {
    
    (multiple <- x %/% ref)
    
    result[, max_power - power + 1] <- multiple
    
    (x <- x - multiple * ref)
    
    (ref <- ref / base)
    
    (power <- power - 1)
  }
  
  result[, max_power + 1] <- x
  
  result
}

# toFactor ---------------------------------------------------------------------

#' Convert to Factor with unique Values as Levels
#' 
#' In contrast to the default behaviour of \code{base::as.factor}, this function
#' uses the unsorted unique values of \code{x} as levels and not sorted unique
#' values.
#' 
#' @param x vector to be converted to factor
#' 
#' @examples 
#' x <- c("b", "c", "a")
#' 
#' as.factor(x) # Levels: a b c
#' 
#' toFactor(x) # Levels: b c a
toFactor <- function(x)
{
  if (! is.factor(x)) {
    
    factor(x, levels = unique(x))
    
  } else {
    
    x
  }
}

# toPositiveIndices ------------------------------------------------------------

#' Negative Indices to Positive Indices
#' 
#' Convert negative indices to positive indices where negative indices are 
#' interpreted as indices counting from the back of the vector
#' 
#' @param indices vector of integer indices
#' @param n maximum index. The index -1 is mapped to \code{n}, the index -2 to
#'   \code{n - 1}, etc.
#' 
toPositiveIndices <- function(indices, n)
{
  negativeIndices <- which(indices < 0)
  indices[negativeIndices] <- indices[negativeIndices] + n + 1
  indices
}

# toInches ---------------------------------------------------------------------

#' Convert Centimeters to Inches
#' 
#' @param cm vector of numeric representing length(s) in cm
#' 
#' @return vector of numeric representing length(s) in inches
#' 
toInches <- function(cm)
{
  cm / 2.54
}

# limitToRange -----------------------------------------------------------------

#' Limit Values to Interval
#'
#' limit the values in \code{x} so that each value lies within the closed
#' interval \code{[left, right]}
#'
#' @param x vector of numeric values
#' @param left lower boundary
#' @param right upper boundary
#' 
#' @examples
#' limitToRange(1:20, left = 5, right = 15)
#' 
limitToRange <- function(x, left = .Machine$double.eps, right = 1.0)
{
  if (length(left) != 1 || length(right) != 1) {
    
    stop("length of left or right is not one")
  }
  
  x[x < left] <- left
  x[x > right] <- right
  
  x
}

# toKeysAndValues --------------------------------------------------------------

#' Key Value String to List of Keys and Values
#' 
#' Converts a string of the form "a=1,b=2" to a list with elements
#'   \code{keys} (here: c("a", "b")) and \code{values} (here: (1,2)).
#' 
#' @param x character vector of length 1
#' @param separators character vector of length 2 representing two types of
#'   separators. The first (default: ",") is used to split \code{x} into single
#'   \code{key = value} assignments. The second (default: "=") is used to split
#'   each assignment into key and value.
#' 
#' @return list with elements \code{keys} and \code{values}
#' 
toKeysAndValues <- function(x, separators = c(",", "="))
{
  stopifnot(is.character(x), length(x) == 1)
  
  assignments <- strsplit(x, separators[1])[[1]]
  
  pairs <- strsplit(assignments, separators[2])
  
  list(
    keys = sapply(pairs, "[", 1),
    values = sapply(pairs, "[", 2)
  )
}

# underscoreToPercent ----------------------------------------------------------

#' Replace underscore with percent sign
#' 
#' Replace underscore with percent sign. May be used to define time format 
#' strings as defaults in function declarations which are not supported by
#' inlinedocs.
#'
#' @param x character vector containing underscores 
underscoreToPercent <- function(x)
{
  gsub("_", "%", x)
}

# toFormula --------------------------------------------------------------------

#' Create Formula from Left and Right Term Strings
#' 
#' create a formula of the form \code{leftterm ~ rightterms[1] + rightterms[2] +
#' ...}
#' 
#' @param leftterm character. Left term of the formula
#' @param rightterms vector of character. Right terms of the formula to be
#'   concatenated with "+".
#' @param as.formula if TRUE (default) the formula object is returned, otherwise
#'   the formula string (character)
#'   
#' @return formula object as generated by \code{formula} or formula string 
#'   (\code{character}) of the form \code{leftterm ~ rightterms[1] + 
#'   rightterms[2] + ...} if \code{as.formula} is FALSE
#' 
#' @examples 
#' f1 <- toFormula("y", c("x1", "x2"))
#' f2 <- toFormula("y", paste0("x", 1:20))
#'   
#' f3 <- toFormula("BMI", c("height", "mass"), as.formula = FALSE)
#'   
#' # f1 and f2 are formulas ...
#' class(f1)
#' class(f2)
#'   
#' # ... but f3 is just "character"
#' class(f3)
#' 
toFormula <- function(leftterm, rightterms, as.formula = TRUE)
{
  leftside <- leftterm
  
  rightside <- paste(rightterms, collapse = " + ")
  
  formulaString <- paste(leftside, "~", rightside)
  
  if (as.formula) {
    
    stats::formula(formulaString)
    
  } else {
    
    formulaString
  }
}

# frenchToAscii ----------------------------------------------------------------

#' French Unicode Letter to ASCII Letter(s)
#' 
#' @return list of ASCII characters (list elements) replacing unicode characters
#'   (element names)
#' 
frenchToAscii <- function()
{
  list(
    "\\xc0" = "A",
    "\\xe0" = "a",
    "\\xc2" = "A",
    "\\xe2" = "a",
    "\\xc6" = "AE",
    "\\xe6" = "ae",
    "\\xc7" = "C",
    "\\xe7" = "c",
    "\\xc8" = "E",
    "\\xe8" = "e",
    "\\xc9" = "E",
    "\\xe9" = "e",
    "\\xca" = "E",
    "\\xea" = "e",
    "\\xcb" = "E",
    "\\xeb" = "e",
    "\\xce" = "I",
    "\\xee" = "i",
    "\\xcf" = "I",
    "\\xef" = "i",
    "\\xd4" = "O",
    "\\xf4" = "o",
    "\\x8c" = "OE",
    "\\x9c" = "oe",
    "\\xd9" = "U",
    "\\xf9" = "u",
    "\\xdb" = "U",
    "\\xfb" = "u",
    "\\xdc" = "U",
    "\\xfc" = "u"
  )  
}

# revertListAssignments --------------------------------------------------------

#' Revert List Assignments
#' 
#' Switch list elements with their names
#' 
#' @param x list of named elements
#'   
#' @return list with the names of \emph{x} as elements and the elements of
#'   \emph{x} as names
#' 
#' @examples 
#' abbreviation <- list(de = "Germany", en = "England")
#'
#' revertListAssignments(abbreviation)
#'
#' ## reverting twice results in the original list
#' identical(
#'   abbreviation, 
#'   revertListAssignments(revertListAssignments(abbreviation))
#' )
#'
revertListAssignments <- function(x)
{
  reverted <- as.list(names(x))
  
  names(reverted) <- as.character(x)
  
  reverted  
}

# hsChrToNum -------------------------------------------------------------------

#' Character to Numeric
#' 
#' Conversion of text representing a number to a valid number
#' 
#' @param x (vector of) text value(s) to be converted to numeric
#' @param country "en" if value(s) in \emph{x} is (are)  given in English format
#'   (decimal point ".", thousands separator ",") or "de" if value is given in
#'   German format (decimal point ",", thousands separator ".").
#' @param stopOnError if TRUE (default) the program stops if any of the given
#'   values could not be converted.
#' 
#' @return vector of numeric(s). In case of conversion the function stops (if 
#'   \code{stopOnError = TRUE}) or returns \code{NA} for those indices for which
#'   the conversion failed. In the latter case an attribute "errorIndices" 
#'   containing the corresponding indices is assigned to the result vector.
#' 
hsChrToNum <- function(x, country, stopOnError = TRUE)
{
  if (! country %in% c("en", "de")) {
    
    stop("country must be one of 'en' (English format) or 'de' (German format)")  
  }
  
  # all elements must contain valid numeric representations in given country
  isValid <- hsValidValue(x, lng = country)
  
  .shortList <- function(x, n = 6L) {
    
    out <- stringList(utils::head(x, n), collapse = ", ")
    
    if (length(x) > n) paste0(out, ", ...") else out
  }
    
  if (! all(isValid)) {
    
    invalidValues <- x[!isValid]
    
    text <- sprintf(
      "%d values are not in acceptable format for country %s: %s", 
      length(invalidValues), country, .shortList(invalidValues)
    )
    
    if (stopOnError) {
      
      stop(text, call. = FALSE)
      
    } else {
      
      warning(text)
    }
  }
  
  # remove thousands separators
  x <- gsub(ifelse(country == "de", "\\.", "\\,"), "", x)
  
  # substitute comma with dot in case of German format
  if (country == "de") {
    
    x <- gsub("\\,", ".", x)
  }
  
  # prepare a numeric result vector
  result <- numeric(length = length(x))
  
  result[isValid] <- as.numeric(x[isValid])
  
  result[! isValid] <- NA
  
  # if there were invalid values return the corresponding indices in the 
  # attribute "errorIndices"
  if (! all(isValid)) {
    
    attr(result, "errorIndices") <- which(!isValid)
  }
  
  result
}

# hsValidValue -----------------------------------------------------------------

#' Value in Correct English/German Notation?
#' 
#' Returns TRUE if text representation of number is in correct format in 
#'   terms of decimal character and (optionally) thousand's separator character.
#'
#' @param x vector of character strings
#' @param lng language code: "en" for English or "de" for German
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) debug messages are
#'   shown
#' @param accept.na if \code{TRUE} (default) \code{TRUE} is returned for 
#'   \code{NA} values within \code{x}
#'    
hsValidValue <- function(x, lng, dbg = FALSE, accept.na = TRUE) 
{
  decim <- ifelse(lng == "de", ",", ".")
  
  thsep <- ifelse(lng == "de", ".", ",")
  
  #@2012-11-12;HS;pattern improved, e.g. "1234.23" was not accepted before
  # number without decimal point or thousand's separator
  p1 <- "\\d*"                         
  
  # number with decimal point only
  p2 <- sprintf("\\d*\\%s\\d*", decim) 
  
  # number with thousand's separator
  p3 <- sprintf("\\d{1,3}(\\%s\\d{3})+(\\%s\\d*)?", thsep, decim) 
  
  ptrn <- sprintf("^([+-]?((%s)|(%s)|(%s)))$", p1, p2, p3)  

  catIf(dbg, "applied pattern: ", ptrn, "\n")    
    
  res <- rep(FALSE, length(x))
  
  res[grep(ptrn, x)] <- TRUE
  
  if (accept.na) {
    
    res[is.na(x)] <- TRUE
  }
  
  res
}

# hsStringToDouble -------------------------------------------------------------

#' Convert String to Double
#' 
#' Convert string to double considering given decimal sign in input string
#' 
#' @param strDbl character string representing a double value
#' @param dec decimal character (default: \code{"."})
#' 
#' @return double representation of input string \code{strDbl}
#' 
hsStringToDouble <- function(strDbl, dec = ".")
{
  msg <- sprintf("Did you use the wrong decimal sign (\"%s\")? ", dec)
  
  # if decimal sign is ".", remove all ","; give a warning if there is exactly
  # one "," to be removed but no "." at all; this may indicate that the wrong 
  # decimal sign was chosen
  if (dec == ".") {
    
    cond <- (
      (hsCountInStr(",", strDbl) == 1) & (hsCountInStr("\\.", strDbl) == 0)
    )
    
    if (any(cond)) {
      
      warning(
        msg, "In the following strings I removed all comma ", 
        "and then converted to double:\n  ", paste(strDbl[cond], collapse = ";")
      )
    }
    
    strDbl <- gsub(",", "", strDbl)
    
  } else if (dec == ",") {
    
    ## if decimal sign is ",", remove all "." and convert "," to "." afterwards.
    ## Give a warning if there is exactly one "." to be removed but no "," 
    ## at all; this may indicate that the wrong decimal sign was chosen
    cond <- (
      (hsCountInStr("\\.", strDbl) == 1) & (hsCountInStr(",", strDbl) == 0)
    )
    
    if (any(cond)) {
      
      warning(
        msg, "In the following strings I removed all dots, ", 
        "substituted comma with dot and then converted to double:\n  ", 
        paste(strDbl[cond], collapse = ";")
      )
    }
    
    strDbl <- gsub(",", ".", gsub("\\.", "", strDbl))
    
  } else {

    stop("Unexpected decimal sign \"", dec, "\".")
  }
  
  # There should be only one "." left!
  cond <- (hsCountInStr("\\.", strDbl) > 1)
  
  if (any(cond)) {

    stop(
      "Cannot convert the following strings to double when dec = \"", 
      dec, "\".\n", "There are multiple decimal dots.\n", 
      paste(strDbl[cond], collapse = "\n")
    )
  }
  
  as.double(strDbl)
}

# hsStringToDate ---------------------------------------------------------------

#' Convert String to Date
#' 
#' Convert date string to string and stop on failure
#' 
#' @param strDate (vector of) string(s) representing date(s)
#' @param dateFormat date format specifier describing the format in which dates
#'   are represented in the csv file. Use placeholders , \code{"\%d"} (day), 
#'   \code{"\%m"} (month), \code{"\%y"} (2-digit year), \code{"\%Y"} (4-digit
#'   year) to describe the date format. \code{"\%d.\%m.\%Y"}, 
#'   \code{"\%d/\%m/\%y"}, \code{"\%Y-\%m-\%d"} are examples for valid format
#'   specifiers.
#' 
#' @return (vector of) Date object(s)
#' 
hsStringToDate <- function(strDate, dateFormat)
{
  # convert string to date
  res <- as.Date(strDate, format = dateFormat)
  
  # Stop if there are NAs in the result vector. Rhe wrong dateFormat may have
  # been used
  if (any(is.na(res))) {
    
    stop(sprintf(
      paste(
        "Error when trying to convert strings to Dates.",
        "Did you use the wrong date format (\"%s\")?\n",
        "E.g. \"%s\" could not be converted to Date.\n",
        "Use placeholders %%d (day), %%m (month), ",
        "%%y (2-digit year), %%Y (4-digit year),",
        "e.g. \"%%d.%%m.%%Y\""
      ),
      dateFormat, strDate[is.na(res)][1]
    ))
  }
  
  res
}

# frenchToAscii ----------------------------------------------------------------
frenchToAscii <- function() # French unicode letter to ASCII letter(s)
### French unicode letter to ASCII letter(s)
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
  
  ### list of ASCII characters (list elements) replacing unicode characters
  ### (element names)
}

# revertListAssignments --------------------------------------------------------
revertListAssignments <- structure(
  function # revertListAssignments
  ### switch list elements with their names
  (
    x
    ### list of named elements
  )
  {
    reverted <- as.list(names(x))
    names(reverted) <- as.character(x)
    
    reverted  
    ### list with the names of \emph{x} as elements and the elements of \emph{x}
    ### as names
  }, ex = function() {
    abbreviation <- list(de = "Germany", en = "England")
    
    revertListAssignments(abbreviation)
    
    ## reverting twice results in the original list
    identical(
      abbreviation, 
      revertListAssignments(revertListAssignments(abbreviation))
    )
  })

# hsChrToNum -------------------------------------------------------------------
hsChrToNum <- function
### conversion of text representing a number to a valid number
(
  x,
  ### (vector of) text value(s) to be converted to numeric
  country,
  ### "en" if value(s) in \emph{x} is (are)  given in English format (decimal
  ### point ".", thousands separator ",") or "de" if value is given in German
  ### format (decimal point ",", thousands separator ".").
  stopOnError = TRUE
  ### if TRUE (default) the program stops if any of the given values could not
  ### be converted.
)
{
  if (! country %in% c("en", "de")) {
    stop("country must be one of 'en' (English format) or 'de' (German format)")  
  }
  
  ## all elements must contain valid numeric representations in given country
  isValid <- hsValidValue(x, lng = country)
  
  if (! all(isValid)) {
    
    invalidValues <- x[!isValid]
    
    messageText <- sprintf(
      "%d values are not in acceptable format for country %s: %s", 
      length(invalidValues), 
      country, 
      paste("\"", invalidValues, "\"", sep = "", collapse = ", "))
    
    if (stopOnError) {
      stop(messageText)  
    }
    else {
      warning(messageText)
    }
  }
  
  ## remove thousands separators
  x <- gsub(ifelse(country == "de", "\\.", "\\,"), "", x)
  
  ## substitute comma with dot in case of German format
  if (country == "de") {
    x <- gsub("\\,", ".", x)
  }
  
  ## prepare a numeric result vector
  result <- numeric(length = length(x))
  
  result[isValid] <- as.numeric(x[isValid])
  result[!isValid] <- NA
  
  ## if there were invalid values return the corresponding indices in the 
  ## attribute "errorIndices"
  if (! all(isValid)) {
    attr(result, "errorIndices") <- which(!isValid)
  }
  
  ### vector of numeric(s). In case of conversion the function stops 
  ### (if stopOnError == TRUE) or returns NA for those indices for which the
  ### conversion failed. In the latter case an attribute "errorIndices" 
  ### containing the corresponding indices is assigned to the result vector.
  result
}

# hsValidValue: value in a correct English/German notation? --------------------
hsValidValue <- function
### returns TRUE if text representation of number is in correct format in 
### terms of decimal character and (optionally) thousand's separator character.
(
  x, 
  lng, 
  dbg = FALSE, 
  accept.na = TRUE
) 
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
  #ptrn <- sprintf("^([+-]?(\\d{1,3}(\\%s\\d{3})*\\%s?)?\\d*)$", thsep, decim)
  
  if (dbg) {
    cat("applied pattern: ", ptrn, "\n")    
  }
  res <- rep(FALSE, length(x))
  res[grep(ptrn, x)] <- TRUE
  if (accept.na) {
    res[is.na(x)] <- TRUE
  }
  res
}

# hsStringToDouble -------------------------------------------------------------
hsStringToDouble <- function # convert string to double
### convert string to double considering given decimal sign in input string
(
  strDbl,
  dec = "."
) 
{
  msg <- sprintf("Did you use the wrong decimal sign (\"%s\")? ", dec)
  
  ## if decimal sign is ".", remove all ","; give a warning if there is exactly
  ## one "," to be removed but no "." at all; this may indicate that the wrong 
  ## decimal sign was chosen
  if (dec == ".") {
    cond <- ((hsCountInStr(  ",", strDbl) == 1) 
             & (hsCountInStr("\\.", strDbl) == 0))
    if (any(cond))
      warning(msg, "In the following strings I removed all comma ", 
              "and then converted to double:\n  ", paste(strDbl[cond], collapse = ";"))
    strDbl <- gsub(",", "", strDbl)    
  }
  else if (dec == ",") {
    ## if decimal sign is ",", remove all "." and convert "," to "." afterwards.
    ## Give a warning if there is exactly one "." to be removed but no "," 
    ## at all; this may indicate that the wrong decimal sign was chosen
    cond <- ((hsCountInStr("\\.", strDbl) == 1) 
             & (hsCountInStr(  ",", strDbl) == 0))
    if (any(cond))
      warning(msg, "In the following strings I removed all dots, ", 
              "substituted comma with dot and then converted to double:\n  ", 
              paste(strDbl[cond], collapse = ";"))
    strDbl <- gsub(",", ".", gsub("\\.", "", strDbl))
  }
  else 
    stop("Unexpected decimal sign \"", dec, "\".")
  
  ## There should be only one "." left!
  cond <- (hsCountInStr("\\.", strDbl) > 1)
  if (any(cond))
    stop("Cannot convert the following strings to double when dec = \"", dec, "\".\n",
         "There are multiple decimal dots.\n", paste(strDbl[cond], collapse = "\n"))
  
  as.double(strDbl)
  ### double representation of input string \emph{strDbl}
}

# hsStringToDate ---------------------------------------------------------------
hsStringToDate <- function
### Convert date string to string and stop on failure
(  
  strDate, 
  ###(vector of) string(s) representing date(s)
  dateFormat
  ### date format specifier describing the format in which dates are represented
  ### in the csv file. Use placeholders , \code{"%d"} (day), 
  ### \code{"%m"} (month), \code{"%y"} (2-digit year), \code{"%Y"}
  ### (4-digit year) to describe the date format. \code{"%d.%m.%Y"}, 
  ### \code{"%d/%m/%y"}, \code{"%Y-%m-%d"} are examples for
  ### valid format specifiers.
)
{
  ## convert string to date
  res <- as.Date(strDate, format = dateFormat)
  
  ## Stop if there are NAs in the result vector. Rhe wrong dateFormat may have
  ## been used
  if (any(is.na(res)))
    stop(sprintf(paste("Error when trying to convert strings to Dates.",
                       "Did you use the wrong date format (\"%s\")?\n",
                       "E.g. \"%s\" could not be converted to Date.\n",
                       "Use placeholders %%d (day), %%m (month), ",
                       "%%y (2-digit year), %%Y (4-digit year),",
                       "e.g. \"%%d.%%m.%%Y\""),
                 dateFormat, strDate[is.na(res)][1]))
  res
  ### (vector of) Date object(s)
}

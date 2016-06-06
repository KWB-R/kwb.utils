# appendSuffix -----------------------------------------------------------------
appendSuffix <- structure(
  function # append suffix to (selected) character values
  ### append suffix to (selected) character values
  (
    values, 
    ### vector of character values to which \emph{suffix} is to be appended
    suffix, 
    ### (character) suffix to be pasted to \emph{values} that are not in
    ### \emph{valuesToOmit}
    valuesToOmit = NULL
    ### vector of values in \emph{values} to which no suffix is to be appended
  )
  {
    doNotOmit <- ! (values %in% valuesToOmit)  
    values[doNotOmit] <- paste0(values[doNotOmit], suffix)
    values
    ### \emph{values} with \emph{suffix} appended to those values that are not in
    ### \emph{valuesToOmit}
  }, 
  ex = function() {
    values <- c("a", "b", "c")
    
    # Append ".1" to all values
    appendSuffix(values, ".1")
    
    # Append ".1" to all values but "c"
    appendSuffix(values, ".1", valuesToOmit = "c")
  })

# hsCountInStr -----------------------------------------------------------------
hsCountInStr <- function(chr, str) 
  ### Count occurrences of \emph{chr} in \emph{str}
{
  g <- gregexpr(chr, str)
  sapply(g, function(x) {sum(x != -1)})
  
  ### number of orrurrences of \emph{char} in \emph{str}
}

# csvTextToDataFrame -----------------------------------------------------------
csvTextToDataFrame <- function # csvTextToDataFrame
### csvTextToDataFrame
(
  text, 
  ### character vector representing lines of comma separated values
  ...
  ### arguments passed to read.table
)
{
  dfr <- read.table(tc <- textConnection(text), ...)
  close(tc)
  dfr
}

# commaCollapsed ---------------------------------------------------------------
commaCollapsed <- function # commaCollapsed
### commaCollapsed
(
  x
)
{
  paste(as.character(x), collapse=",")
}

# hsQuoteChr -------------------------------------------------------------------
hsQuoteChr <- function # hsQuoteChr
### quotes objects of type character with quoting character
(
  x,
  ### vector or list of elements
  qchar = "'",
  ### quoting character to be used. Default: single quote "'"
  escapeMethod = c("double", "backslash", "none")
) 
{
  if (mode(x) == "list") {
    len <- length(x)
    if (len == 0) return(NULL)
    if (len == 1) return(hsQuoteChr(x[[1]]))
    return(c(hsQuoteChr(x[[1]]), hsQuoteChr(x[-1])))
  }
  
  escapeChars <- list(double = qchar, backslash = '\\', none = "")
  
  if (mode(x) == "character") {
    escapeChar <- escapeChars[[escapeMethod[1]]]
    x.escaped <- gsub(qchar, paste(escapeChar, qchar, sep=""), x)
    return(paste(qchar, x.escaped, qchar, sep = ""))
  }
  
  return(x)
}

# multiSubstitute --------------------------------------------------------------
multiSubstitute <- function # multiple substitutions
### apply multiple substitutions on a vector of character. For each element in
### \emph{replacements} gsub is called with the element name being the pattern
### and the element value being the replacement.
(
  strings,
  ### vector of character
  replacements,
  ### list of pattern = replacement pairs. 
  ...
  ### additional arguments passed to gsub
)
{
  for (pattern in names(replacements)) {
    strings <- gsub(pattern, replacements[[pattern]], strings, ...)
  }
  
  strings
}

# hsTrim -----------------------------------------------------------------------
hsTrim <- function # Remove leading and trailing spaces
###  Remove leading, trailing (and, if requested, duplicate) spaces
(
  str,
  ### vector of character containing the strings to be trimmed
  trim.multiple.spaces = TRUE
  ### if TRUE (default), multiple consecutive spaces are replaced by one space
) 
{
  replacements <- list(
    "^\\s+" = "", 
    "\\s+$" = ""
  )
  
  if (trim.multiple.spaces) {
    replacements <- c(replacements, "\\s+" = " ")
  }
  
  multiSubstitute(str, replacements)  
  
  ### input string \emph{str} without leading or trailing spaces and with
  ### multiple consecutive spaces being replaced by a single space
}

# removeSpaces -----------------------------------------------------------------
removeSpaces <- function # remove all spaces in string(s)
### remove all spaces in string(s)
(
  x
  ### (vector of) character
)
{
  gsub("\\s+", "", x)
  ### \emph{x} with all spaces removed
}

# hsSubstSpecChars -------------------------------------------------------------
hsSubstSpecChars <- function
### Substitution of special characters
#@2012-11-20;moved from kwb.misc::hsLibBase.R
(
  x
  ### string containing special characters to be substituted
) 
{  
  replacements <- list(
    "\xe4" = "ae",
    "\xf6" = "oe", 
    "\xfc" = "ue",
    "\xdf" = "ss",
    "\xb5" = "my",
    "%" = "proz",
    "\\\\" = "_",        # Replace backslash with underscore
    "[() -/.,;?]" = "_", # Replace special characters with underscore
    "\\[" = "_",         # Replace opening bracket with underscore
    "\\]" = "_",         # Replace closing bracket with underscore
    "_+" = "_",          # Replace multiple underscores by one underscore
    "_$" = ""            # Remove underscore at the end
  )  
  
  multiSubstitute(x, replacements)
  ### input string \emph{x} with special characters being substituted by 
  ### a meaningful represenation or underscore, multiple underscores replaced
  ### by a single underscore and multiple underscores at the end removed.
}

# commaCollapsed 

# stringToExpression -----------------------------------------------------------
stringToExpression <- function # stringToExpression
### stringToExpression
(
  expressionString
)
{
  parse(text=expressionString)
}

# stringContains ---------------------------------------------------------------
stringContains <- structure(
  function # stringContains
  ### stringContains
  (
    x, 
    contains
  )
  {
    (1:length(x)) %in% grep(contains, x)
  }, ex=function() {
    stringContains(c("abc", "Kabeljau", "Arabella"), "ab")
    stringContains(c("abc", "Kabeljau", "Arabella"), "abc")
  })

# stringStartsWith -------------------------------------------------------------
stringStartsWith <- structure(
  function # stringStartsWith
  ### stringStartsWith
  (
    x, 
    startsWith
    ### string to be searched for at the beginning of the string(s) in \emph{x}
  )
  {
    (1:length(x)) %in% grep(paste("^", startsWith, sep=""), x)
  }, ex=function() {
    stringStartsWith(c("abc", "Kabeljau", "Arabella"), "ab")
    stringStartsWith(c("abc", "Kabeljau", "Arabella"), "A")
  })

# stringEndsWith ---------------------------------------------------------------
stringEndsWith <- structure(
  function # stringEndsWith
  ### stringEndsWith
  (
    x, 
    endsWith
    ### string to be searched for at the end of the string(s) in \emph{x}  
  )
  {
    (1:length(x)) %in% grep(paste(endsWith, "$", sep=""), x)
  }, ex=function() {
    stringEndsWith(c("abc", "Kabeljau", "Arabella"), "a")
    stringEndsWith(c("abc", "Kabeljau", "Arabella"), "jau")
  })

# .test_subExpressionMatches ---------------------------------------------------
.test_subExpressionMatches <- function()
{
  y1 <- subExpressionMatches(
    regularExpression = "(\\d{4})-(\\d{2})-(\\d{2})", 
    text = c("1975-01-14", "2003-01", "2015-08-20"),
    match.names = c("year", "month", "day")
  )
  
  expected1 <- list(
    list(year = "1975", month = "01", day = "14"),
    NULL,
    list(year = "2015", month = "08", day = "20")
  )
  
  y2 <- subExpressionMatches(
    regularExpression = "^([^.]+)\\.([^.]+)@(.*)$",
    text = c("hauke.sonnenberg@lernshow.de",
             "angela.merkel@germany.de"),
    match.names = c("firstName", "lastName", "host")
  )
  
  expected2 <- list(
    list(firstName = "hauke", lastName = "sonnenberg", host = "lernshow.de"),
    list(firstName = "angela", lastName = "merkel", host = "germany.de")
  )
  
  pattern <- "^(.*)\\.([^.]+)$"
  match.names = c("basename", "extension")
  
  y3 <- subExpressionMatches(pattern, "file.txt", match.names)
  y4 <- subExpressionMatches(pattern, "file.txt")
  
  expected3 <- list(basename = "file", extension = "txt")
  
  y5 <- subExpressionMatches(
    regularExpression = "(Spieler|Player)\\s+(\\d+)", 
    text = c("Spieler 1", "Player 21", "Spieler 311"),
    select = c(playerID = 2)
  )
  
  expected5 <- list(
    list(playerID = "1"),
    list(playerID = "21"),
    list(playerID = "311")
  )
  
  identical(y1, expected1) && 
    identical(y2, expected2) && 
    identical(y3, expected3) && 
    identical(y4, structure(expected3, names = NULL)) &&
    identical(y5, expected5)
}

# subExpressionMatches ---------------------------------------------------------
subExpressionMatches <- structure(
  function # subExpressionMatches
  ### subExpressionMatches
  (
    regularExpression, 
    ### regular expression containing parts in parentheses that are to be 
    ### extracted from \emph{text}
    text, 
    ### text to be matched against the regular expression
    match.names = NULL,
    ### optional. Names that are to be given to the extracted parts in the result
    ### list,
    select = structure(seq_along(match.names), names = match.names),
    ### named vector of numbers specifying the subexpressions in parentheses to
    ### be extracted.
    simplify = TRUE
    ### if TRUE (default) and \emph{text} has only one element, the output 
    ### structure will be a list instead a list of lists
    
  )
  {
    match.infos <- regexec(regularExpression, text)
    
    x <- lapply(seq_len(length(match.infos)), function(i) {
      
      match.info <- match.infos[[i]]
      
      if (match.info[1] != -1) {
        
        start.pos <- match.info[-1]
        match.len <- attr(match.info, "match.length")[-1]
        stop.pos <- start.pos + match.len - 1
        
        matches <- lapply(seq_len(length(start.pos)), FUN = function(j) {
          substr(text[i], start.pos[j], stop.pos[j])
        })
        
        # If numbers of subexpressions to select are given, select and name the
        # corresponding subexpressions
        if (length(select) > 0) {
          matches <- matches[select]
          names(matches) <- names(select)
        }
        else {
          names(matches) = match.names  
        }        
      }
      else {
        matches <- NULL
      }
      
      matches  
    })
    
    if (simplify && length(x) == 1) {
      x <- x[[1]]
    }
    
    x    
    ### If \code{length(text) > 1} a list is returned with as many 
    ### elements as there are strings in \emph{text} each of which is itself a 
    ### list containing the strings matching the subpatterns (enclosed in 
    ### parentheses in \emph{regularExpression}) or NULL for strings that did
    ### not match. If \emph{match.names} are given, the elements of these lists
    ### are named according to the names given in \emph{match.names}. If
    ### \emph{text} is of length 1 and \emph{simplify} = TRUE (default) the top
    ### level list structure described above is omitted, i.e. the list of
    ### substrings matching the subpatterns is returned.
  }, ex=function() {
    # split date into year, month and day
    subExpressionMatches("(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23")
    
    # split date into year, month and day (give names to the resulting elements)
    x <- subExpressionMatches(
      regularExpression = "(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23",
      match.names = c("year", "month", "day")
    )
    
    cat(paste("Today is ", x$day, "/", x$month, " of ", x$year, "\n", sep=""))
  })

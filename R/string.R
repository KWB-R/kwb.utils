# fileExtension ----------------------------------------------------------------

#' Get Extension of Full File Paths
#' 
#' @param x vector of file paths
#' @export
#' 
#' @examples 
#' # Define example paths
#' paths <- c("C:/example/file.csv", "file2.txt", "D:/e/f/ghi.jkl.zip")
#' 
#' # Get the file name extensions
#' fileExtension(paths)
#' 
#' # Empty string is returned for paths without file name extensions
#' fileExtension("C:/NEWS")
fileExtension <- function(x)
{
  x <- basename(x)
  
  match_infos <- regexec("\\.[^.]+$", x)
  
  sapply(seq_along(match_infos), function(i) {
    
    pos <- match_infos[[i]]
    
    if (pos == -1) {
      ""
    } else {
      substr(x[[i]], pos + 1, nchar(x[[i]]))
    }
  })
}

# pairwise ---------------------------------------------------------------------

#' reorder strings so that matching strings are neighbours
#' 
#' reorder strings so that strings that start with the same characters
#'   appear next to each other
#' 
#' @param x vector of character
#' @param starts vector of character defining the start strings that are looked for in 
#'   \code{x} to find strings that belong together. The default is to take
#'   the unique strings appearing before a \code{split} character (if any)
#' @param split split character used to create default \code{start} strings
#' 
#' @examples 
#'   x <- c("a.1", "b_hi", "c", "a.2", "d", "b_bye")
#'   
#'   # You have the most control when setting the starts argument
#'   pairwise(x, starts = c("a.", "b_"))
#'   
#'   # Use default starts resulting from splitting at a split character
#'   pairwise(x, split = "_")
#'   
#'   # This is actually the default
#'   pairwise(x)
#'   
#'   # Note that the split parameter is interpreted as a pattern where the
#'   # dot has a special meaning unless it is escaped or enclosed in []
#'   pairwise(x, split = "[.]")
#'   
#'   # Same result as in the first example
#'   pairwise(x, split = "[._]")
#'   
#' 
pairwise <- structure(
  function # reorder strings so that matching strings are neighbours
  ### reorder strings so that strings that start with the same characters
  ### appear next to each other
  (
    x, 
    ### vector of character
    starts = .defaultStarts(x, split), 
    ### vector of character defining the start strings that are looked for in 
    ### \code{x} to find strings that belong together. The default is to take
    ### the unique strings appearing before a \code{split} character (if any)
    split = "_"
    ### split character used to create default \code{start} strings
  )
  {
    # Generate column order so that duplicated columns appear next to each other
    columns <- character()
    
    # Do while x is not empty
    while(length(x) > 0) {
      
      # Do the given start strings match the currently first element of x?
      match <- sapply(starts, function(start) {stringStartsWith(x[1], start)})
      
      # If there is a match find the positions in x at which the elements start
      # with the same string as given in the first matching start
      if (any(match)) {
        positions <- which(stringStartsWith(x, names(which(match)[1])))
      } else {
        positions <- 1
      }
      
      # Add the elements of x at the given positions to the result vector
      columns <- c(columns, x[positions])
      
      # ... and remove the elements at the given positions from x
      x <- x[-positions]
    }
    
    columns
  }, ex = function() {
    x <- c("a.1", "b_hi", "c", "a.2", "d", "b_bye")
    
    # You have the most control when setting the starts argument
    pairwise(x, starts = c("a.", "b_"))
    
    # Use default starts resulting from splitting at a split character
    pairwise(x, split = "_")
    
    # This is actually the default
    pairwise(x)
    
    # Note that the split parameter is interpreted as a pattern where the
    # dot has a special meaning unless it is escaped or enclosed in []
    pairwise(x, split = "[.]")
    
    # Same result as in the first example
    pairwise(x, split = "[._]")
  })

# .defaultStarts ---------------------------------------------------------------

#'  defaultStarts
#' 
#' 
.defaultStarts <- function(x, split = "_")
{
  # split the strings
  parts <- strsplit(x, split)
  unique(sapply(parts[sapply(parts, length) > 1], "[", 1))
}

# appendSuffix -----------------------------------------------------------------

#' append suffix to (selected) character values
#' 
#' append suffix to (selected) character values
#' 
#' @param values vector of character values to which \emph{suffix} is to be appended
#' @param suffix (character) suffix to be pasted to \emph{values} that are not in
#'   \emph{valuesToOmit}
#' @param valuesToOmit vector of values in \emph{values} to which no suffix is to be appended
#' 
#' @return \emph{values} with \emph{suffix} appended to those values that are not in
#'   \emph{valuesToOmit}
#' 
#' @examples 
#'   values <- c("a", "b", "c")
#'   
#'   # Append ".1" to all values
#'   appendSuffix(values, ".1")
#'   
#'   # Append ".1" to all values but "c"
#'   appendSuffix(values, ".1", valuesToOmit = "c")
#'   
#' 
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

#' Count Pattern in String
#' 
#' Count occurrences of \code{chr} in \code{str}
#' 
#' @param chr character string (or pattern) to be looked for and counted in 
#'   \code{str}
#' @param str character string in which to count for \code{chr} 
#' @return number of occurrences of \code{char} in \code{str}
#' 
hsCountInStr <- function(chr, str) 
{
  g <- gregexpr(chr, str)
  sapply(g, function(x) {sum(x != -1)})
}

# csvTextToDataFrame -----------------------------------------------------------

#' csvTextToDataFrame
#' 
#' csvTextToDataFrame
#' 
#' @param text character vector representing lines of comma separated values
#' @param \dots arguments passed to \code{utils::read.table}
#' 
csvTextToDataFrame <- function # csvTextToDataFrame
### csvTextToDataFrame
(
  text, 
  ### character vector representing lines of comma separated values
  ...
  ### arguments passed to \code{utils::read.table}
)
{
  utils::read.table(text = text, ...)
}

# stringList -------------------------------------------------------------------

#' string of comma separated quoted strings
#' 
#' create a string of comma separated quoted strings
#' 
#' @param x vector of character
#' @param qchar character to be used for quoting, default: single quote character
#' @param collapse characters used to separate the strings. Default: ", "
#' 
stringList <- function(x, qchar = "'", collapse = ", ") 
{
  collapsed(hsQuoteChr(x, qchar = qchar), collapse = collapse)
}

# commaCollapsed ---------------------------------------------------------------

#' Paste With Collapse = ","
#' 
#' paste with collapse = ","
#' 
#' @param x vector of character

commaCollapsed <- function
(
  x
)
{
  collapsed(x, ",")
}

# collapsed --------------------------------------------------------------------

#' shortcut to paste(x, collapse = collapse)
#' 
#' This is just a shortcut to paste(x, collapse = collapse)
#'
#' @param x vector of character
#' @param collapse character string to separate the elements in x (passed to 
#'   \code{\link[base]{paste}})
#'   
collapsed <- function # shortcut to paste(x, collapse = collapse)
### This is just a shortcut to paste(x, collapse = collapse)
(
  x,
  collapse = " "
)
{
    paste(as.character(x), collapse = collapse)
}

# hsQuoteChr -------------------------------------------------------------------

#' Quote Character Strings
#' 
#' quotes objects of type character with quoting character
#' 
#' @param x vector or list of character strings
#' @param qchar quoting character to be used. Default: single quote "'"
#' @param escapeMethod one of \code{"double", "backslash", "none"} deciding
#'   how to treat the quote character if it occurs within the string to be
#'   quoted
#' 
hsQuoteChr <- function(
  x,
  qchar = "'",
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

#' Multiple Substitutions
#' 
#' apply multiple substitutions on a vector of character. For each element in
#'   \emph{replacements} gsub is called with the element name being the pattern
#'   and the element value being the replacement.
#' 
#' @param strings vector of character
#' @param replacements list of pattern = replacement pairs. 
#' @param \dots additional arguments passed to gsub
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) it is shown which
#'   strings were replaced
#'   
multiSubstitute <- function(strings, replacements, ..., dbg = FALSE)
{
  for (pattern in names(replacements)) {
    
    if (dbg) {
      strings.bak <- strings
    }
    
    replacement <- replacements[[pattern]]
    strings <- gsub(pattern, replacement, strings, ...)
    
    if (dbg) {
      
      changed <- strings != strings.bak
      
      if (any(changed)) {
        
        frequencies <- table(strings.bak[changed])
        items <- sprintf("'%s' (%d-times)", names(frequencies), frequencies)
        
        cat(sprintf(
          paste0("In the following strings the parts matching the pattern ", 
                 "'%s' are replaced with '%s':\n  %s\n"), 
          pattern, replacement, collapsed(items, ",\n  ")
        ))
      }
    }
  }
  
  strings
}

# hsTrim -----------------------------------------------------------------------

#' Remove leading and trailing spaces
#' 
#' Remove leading, trailing (and, if requested, duplicate) spaces
#' 
#' @param str vector of character containing the strings to be trimmed
#' @param trim.multiple.spaces if TRUE (default), multiple consecutive spaces are replaced by one space
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) debut messages
#'   are shown
#' 
#' @return input string \emph{str} without leading or trailing spaces and with
#'   multiple consecutive spaces being replaced by a single space
#' 
hsTrim <- function(
  str,
  trim.multiple.spaces = TRUE,
  dbg = FALSE
) 
{
  replacements <- list(
    "^\\s+" = "", 
    "\\s+$" = ""
  )
  
  if (trim.multiple.spaces) {
    replacements <- c(replacements, "\\s+" = " ")
  }
  
  multiSubstitute(str, replacements, dbg = dbg)
  
  ### input string \emph{str} without leading or trailing spaces and with
  ### multiple consecutive spaces being replaced by a single space
}

# removeSpaces -----------------------------------------------------------------

#' remove all spaces in string(s)
#' 
#' remove all spaces in string(s)
#' 
#' @param x (vector of) character
#' 
#' @return \emph{x} with all spaces removed
#' 
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

#' Substitution of Special Characters
#' 
#' Substitution of special characters
#' 
#' @param x string containing special characters to be substituted
#' @return input string \emph{x} with special characters being substituted by 
#'   a meaningful represenation or underscore, multiple underscores replaced
#'   by a single underscore and multiple underscores at the end removed.
#' 
hsSubstSpecChars <- function(x)
{
  replacements.x <- list(
    "\\xe4" = "ae", "\\xf6" = "oe", "\\xfc" = "ue", "\\xdf" = "ss", 
    "\\xb5" = "my"
  )
  
  replacements.other <- list(
    "%" = "proz",
    "\\\\" = "_",        # Replace backslash with underscore
    "[() -/.,;?]" = "_", # Replace special characters with underscore
    "\\[" = "_",         # Replace opening bracket with underscore
    "\\]" = "_",         # Replace closing bracket with underscore
    "_+" = "_",          # Replace multiple underscores by one underscore
    "_$" = ""            # Remove underscore at the end
  )
  
  multiSubstitute(x, c(replacements.x, replacements.other))
}

# stringToExpression -----------------------------------------------------------

#' Convert String to Expression
#' 
#' @param expressionString character string to be converted to an expression
stringToExpression <- function(expressionString)
{
  parse(text = expressionString)
}

# stringContains ---------------------------------------------------------------

#' stringContains
#' 
#' stringContains
#'
#' @param x vector of character
#' @param contains vector of character
#'  
#' @examples 
#'   stringContains(c("abc", "Kabeljau", "Arabella"), "ab")
#'   stringContains(c("abc", "Kabeljau", "Arabella"), "abc")
#'   
stringContains <- function(x, contains)
{
  seq_along(x) %in% grep(contains, x)
}

# stringStartsWith -------------------------------------------------------------

#' stringStartsWith
#' 
#' stringStartsWith
#' 
#' @param x vector of character to be checked if they start with
#'   \code{startsWith}
#' @param startsWith string to be searched for at the beginning of the string(s)
#'   in \code{x}
#'   
#' @examples 
#'   stringStartsWith(c("abc", "Kabeljau", "Arabella"), "ab")
#'   stringStartsWith(c("abc", "Kabeljau", "Arabella"), "A")
#'   
stringStartsWith <- function(x, startsWith)
{
  seq_along(x) %in% grep(paste("^", startsWith, sep=""), x)
}

# stringEndsWith ---------------------------------------------------------------

#' stringEndsWith
#' 
#' stringEndsWith
#' 
#' @param x vector of character to be checked if they end with \code{endsWith}
#' @param endsWith string to be searched for at the end of the string(s) in
#'   \code{x}
#' 
#' @examples 
#'   stringEndsWith(c("abc", "Kabeljau", "Arabella"), "a")
#'   stringEndsWith(c("abc", "Kabeljau", "Arabella"), "jau")
#'   
stringEndsWith <- function(x, endsWith)
{
  seq_along(x) %in% grep(paste(endsWith, "$", sep=""), x)
}

# .test_subExpressionMatches ---------------------------------------------------

#'  test subExpressionMatches
#' 
#' 
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

# extractSubstring -------------------------------------------------------------

#' extract substrings defined by regular expressions
#' 
#' extract substrings defined by regular expressions from a vector of strings
#' 
#' @param pattern regular expression containing parts in pairs of opening and closing 
#'   parentheses defining the part(s) to be extracted
#' @param x vector of character strings
#' @param index index(es) of parenthesized subexpression(s) to be extracted. If the length
#'   of \code{x} is greater than one a data frame is returned with each column
#'   containing the substrings matching the subexpression at the corresponding
#'   index. If \code{index} is named, the names will be used as column names.
#' @param stringsAsFactors if \code{TRUE} (default is {FALSE}) and a data frame is returned then the
#'   columns in the returned data frame are of factors, otherwise vectors of
#'   character.
#' 
#' @examples 
#'   # Define pattern matching a date
#'   pattern <- "([^ ]+), ([0-9]+) of ([^ ]+)"
#'   
#'   # Extract single sub expressions from one string
#'   datestring <- "Thursday, 8 of December"
#'   extractSubstring(pattern, datestring, 1) # ""Thursday""
#'   extractSubstring(pattern, datestring, 2) # "8"
#'   extractSubstring(pattern, datestring, 3) # "December"
#'   
#'   # Extract single sub expressions from a vector of strings
#'   datestrings <- c("Thursday, 8 of December", "Tuesday, 14 of January")
#'   extractSubstring(pattern, datestrings, 1) # "Thursday" "Tuesday"
#'   extractSubstring(pattern, datestrings, 2) # "8"  "14"
#'   extractSubstring(pattern, datestrings, 3) # "December" "January" 
#'   
#'   # Extract more than one subexpression at once -> data.frame
#'   extractSubstring(pattern, datestrings, 1:3)
#'   
#'   #   subexp.1 subexp.2 subexp.3
#'   #   1 Thursday        8 December
#'   #   2  Tuesday       14  January
#'   
#'   # Name the sub expressions by naming their number in index (3rd argument)
#'   extractSubstring(pattern, datestrings, index = c(weekday = 1, 2, month = 3))
#'   #   weekday subexp.2    month
#'   #   1 Thursday        8 December
#'   #   2  Tuesday       14  January
#'   
#' 
extractSubstring <- structure(
  function # extract substrings defined by regular expressions
  ### extract substrings defined by regular expressions from a vector of strings
(
  pattern, 
  ### regular expression containing parts in pairs of opening and closing 
  ### parentheses defining the part(s) to be extracted
  x, 
  ### vector of character strings
  index,
  ### index(es) of parenthesized subexpression(s) to be extracted. If the length
  ### of \code{x} is greater than one a data frame is returned with each column
  ### containing the substrings matching the subexpression at the corresponding
  ### index. If \code{index} is named, the names will be used as column names.
  stringsAsFactors = FALSE
  ### if \code{TRUE} (default is {FALSE}) and a data frame is returned then the
  ### columns in the returned data frame are of factors, otherwise vectors of
  ### character.
)
{
  if (length(index) > 1) {
    names.index <- defaultIfNULL(names(index), rep("", length(index)))
    names.default <- paste0("subexp.", seq_along(index))
    
    isEmpty <- names.index == ""
    names.index[isEmpty] <- names.default[isEmpty]
    
    names(index) <- names.index

    args <- lapply(index, extractSubstring, pattern = pattern, x = x)
    
    callWithStringsAsFactors(stringsAsFactors, data.frame, args)
  }
  else {
    parts <- subExpressionMatches(pattern, x, simplify = FALSE)
    sapply(parts, function(p) defaultIfNULL(p[[index]], ""))
  }
  
}, ex = function() {
  # Define pattern matching a date
  pattern <- "([^ ]+), ([0-9]+) of ([^ ]+)"
  
  # Extract single sub expressions from one string
  datestring <- "Thursday, 8 of December"
  extractSubstring(pattern, datestring, 1) # ""Thursday""
  extractSubstring(pattern, datestring, 2) # "8"
  extractSubstring(pattern, datestring, 3) # "December"
  
  # Extract single sub expressions from a vector of strings
  datestrings <- c("Thursday, 8 of December", "Tuesday, 14 of January")
  extractSubstring(pattern, datestrings, 1) # "Thursday" "Tuesday"
  extractSubstring(pattern, datestrings, 2) # "8"  "14"
  extractSubstring(pattern, datestrings, 3) # "December" "January" 
  
  # Extract more than one subexpression at once -> data.frame
  extractSubstring(pattern, datestrings, 1:3)
  
  #   subexp.1 subexp.2 subexp.3
  #   1 Thursday        8 December
  #   2  Tuesday       14  January
  
  # Name the sub expressions by naming their number in index (3rd argument)
  extractSubstring(pattern, datestrings, index = c(weekday = 1, 2, month = 3))
  #   weekday subexp.2    month
  #   1 Thursday        8 December
  #   2  Tuesday       14  January
})

# subExpressionMatches ---------------------------------------------------------

#' find and extract regular expressions from strings
#' 
#' find and extract regular expressions from strings
#' 
#' @param regularExpression regular expression containing parts in parentheses that are to be 
#'   extracted from \emph{text}
#' @param text text to be matched against the regular expression
#' @param match.names optional. Names that are to be given to the extracted parts in the result
#'   list,
#' @param select named vector of numbers specifying the subexpressions in parentheses to
#'   be extracted.
#' @param simplify if TRUE (default) and \emph{text} has only one element, the output 
#'   structure will be a list instead a list of lists
#' 
#' @return If \code{length(text) > 1} a list is returned with as many 
#'   elements as there are strings in \emph{text} each of which is itself a 
#'   list containing the strings matching the subpatterns (enclosed in 
#'   parentheses in \emph{regularExpression}) or NULL for strings that did
#'   not match. If \emph{match.names} are given, the elements of these lists
#'   are named according to the names given in \emph{match.names}. If
#'   \emph{text} is of length 1 and \emph{simplify} = TRUE (default) the top
#'   level list structure described above is omitted, i.e. the list of
#'   substrings matching the subpatterns is returned.
#' 
#' @examples 
#'   # split date into year, month and day
#'   subExpressionMatches("(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23")
#'   
#'   # split date into year, month and day (give names to the resulting elements)
#'   x <- subExpressionMatches(
#'     regularExpression = "(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23",
#'     match.names = c("year", "month", "day")
#'   )
#'   
#'   cat(paste("Today is ", x$day, "/", x$month, " of ", x$year, "\n", sep=""))
#'   
#' 
subExpressionMatches <- structure(
  function # find and extract regular expressions from strings
  ### find and extract regular expressions from strings
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
    
    x <- lapply(seq_along(match.infos), function(i) {
      
      match.info <- match.infos[[i]]
      
      if (match.info[1] != -1) {
        
        start.pos <- match.info[-1]
        match.len <- attr(match.info, "match.length")[-1]
        stop.pos <- start.pos + match.len - 1
        
        matches <- lapply(seq_along(start.pos), FUN = function(j) {
          substr(text[i], start.pos[j], stop.pos[j])
        })
        
        # If numbers of subexpressions to select are given, select and name the
        # corresponding subexpressions
        if (length(select) > 0) {
          matches <- matches[select]
          names(matches) <- names(select)
        }
        else {
          names(matches) <- match.names  
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
  }, ex = function() {
    # split date into year, month and day
    subExpressionMatches("(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23")
    
    # split date into year, month and day (give names to the resulting elements)
    x <- subExpressionMatches(
      regularExpression = "(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23",
      match.names = c("year", "month", "day")
    )
    
    cat(paste("Today is ", x$day, "/", x$month, " of ", x$year, "\n", sep=""))
  })

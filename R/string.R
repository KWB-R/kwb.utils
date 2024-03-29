# createIdAlong ----------------------------------------------------------------

#' Create Unique Identifiers along Input Vector
#' 
#' This function helps you create strings that can be used as identifiers. 
#' 
#' @param x vector of objects for which to create identifiers
#' @param base_name base string to which the hexadecimal suffixes are to be
#'   appended. By default the name of the object \code{x}, with "s" at the
#'   end removed, is used. For example, passing a vector called "files" to this
#'   function results in ids "file_01", "file_02", ...
#' @return vector of character as long as \code{x}
#' @export
#' @examples 
#' # Create ids for 32 numbers
#' createIdAlong(1:32, "number")
#' # Take base name from x, with the plural's "s" removed
#' numbers <- 1:32
#' createIdAlong(numbers)
createIdAlong <- function(x, base_name = NULL)
{
  base_name <- defaultIfNULL(base_name, gsub("s$", "", deparse(substitute(x))))
  
  sprintf("%s_%02x", base_name, seq_along(x))
}

# removeExtension --------------------------------------------------------------

#' Remove File Name Extension
#' 
#' @param x vector of character
#' @export
#' @examples
#' removeExtension("example.R")
#' removeExtension("any/path/example.txt")
#' 
removeExtension <- function(x)
{
  sub("\\.[^.]+$", "", x)
}

# shorten ----------------------------------------------------------------------

#' Shorten Strings to a Maximum Length
#' 
#' @param x vector of character
#' @param max_chars maximum number of characters to which the strings in 
#'   \code{x} are to be shortened
#' @param delimiter string to be used as separater between the start and the end 
#'   of the strings in \code{x} that are longer than \code{max_chars} characters
#' @return \code{x} with strings longer than \code{max_chars} characters being
#'   shortend by replacing characters in the centre by the \code{delimiter}
#'   string.
#' @export
#' 
shorten <- function(x, max_chars = 10, delimiter = "...")
{
  stopifnot(is.character(x))
  
  too_long <- nchar(x) > max_chars
  
  n_delimiter <- nchar(delimiter)
  
  n_available <- max_chars - n_delimiter
  
  n_left <- ceiling(n_available / 2)
  
  n_right <- n_available - n_left
  
  stopifnot(n_left + n_right + n_delimiter == max_chars)
  
  n <- nchar(x[too_long])
  
  left_side <- substr(x[too_long], 1, n_left)
  
  right_side <- substr(x[too_long], n - n_right + 1, n)
  
  x[too_long] <- paste0(left_side, delimiter, right_side)
  
  x
}

# fileExtension ----------------------------------------------------------------

#' Get Extension of Full File Paths
#' 
#' @param x vector of file paths
#' @export
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
    
    if (pos == -1) "" else substr(x[[i]], pos + 1, nchar(x[[i]]))
  })
}

# pairwise ---------------------------------------------------------------------

#' Reorder Strings So That Matching Strings are Neighbours
#' 
#' Reorder strings so that strings that start with the same characters appear 
#' next to each other
#' 
#' @param x vector of character
#' @param starts vector of character defining the start strings that are looked
#'   for in \code{x} to find strings that belong together. The default is to
#'   take the unique strings appearing before a \code{split} character (if any)
#' @param split split character used to create default \code{start} strings
#' @export
#' @examples 
#' x <- c("a.1", "b_hi", "c", "a.2", "d", "b_bye")
#' 
#' # You have the most control when setting the starts argument
#' pairwise(x, starts = c("a.", "b_"))
#' 
#' # Use default starts resulting from splitting at a split character
#' pairwise(x, split = "_")
#' 
#' # This is actually the default
#' pairwise(x)
#' 
#' # Note that the split parameter is interpreted as a pattern where the
#' # dot has a special meaning unless it is escaped or enclosed in []
#' pairwise(x, split = "[.]")
#' 
#' # Same result as in the first example
#' pairwise(x, split = "[._]")
#' 
pairwise <- function(x, starts = defaultStarts(x, split), split = "_")
{
  # Generate column order so that duplicated columns appear next to each other
  columns <- character()
  
  # Do while x is not empty
  while (length(x) > 0) {
    
    # Do the given start strings match the currently first element of x?
    match <- sapply(starts, stringStartsWith, x = x[1])
    
    # If there is a match find the positions in x at which the elements start
    # with the same string as given in the first matching start
    positions <- if (any(match)) {
      
      which(stringStartsWith(x, names(which(match)[1])))
      
    } else {
      
      1
    }
    
    # Add the elements of x at the given positions to the result vector
    columns <- c(columns, x[positions])
    
    # ... and remove the elements at the given positions from x
    x <- x[-positions]
  }
  
  columns
}

# defaultStarts ----------------------------------------------------------------

defaultStarts <- function(x, split = "_")
{
  # split the strings
  parts <- strsplit(x, split)
  
  unique(sapply(parts[sapply(parts, length) > 1], "[", 1))
}

# appendSuffix -----------------------------------------------------------------

#' Append Suffix to (Selected) Character Values
#' 
#' @param values vector of character values to which \emph{suffix} is to be
#'   appended
#' @param suffix (character) suffix to be pasted to \emph{values} that are not
#'   in \emph{valuesToOmit}
#' @param valuesToOmit vector of values in \emph{values} to which no suffix is
#'   to be appended
#' @return \emph{values} with \emph{suffix} appended to those values that are
#'   not in \emph{valuesToOmit}
#' @export
#' @examples 
#' values <- c("a", "b", "c")
#'   
#' # Append ".1" to all values
#' appendSuffix(values, ".1")
#'   
#' # Append ".1" to all values but "c"
#' appendSuffix(values, ".1", valuesToOmit = "c")
#'   
appendSuffix <- function(values, suffix, valuesToOmit = NULL)
{
  doNotOmit <- ! (values %in% valuesToOmit)  
  
  values[doNotOmit] <- paste0(values[doNotOmit], suffix)
  
  values
}

# hsCountInStr -----------------------------------------------------------------

#' Count Pattern in String
#' 
#' Count occurrences of \code{chr} in \code{str}
#' 
#' @param chr character string (or pattern) to be looked for and counted in 
#'   \code{str}
#' @param str character string in which to count for \code{chr} 
#' @return number of occurrences of \code{char} in \code{str}
#' @export
#' 
hsCountInStr <- function(chr, str) 
{
  g <- gregexpr(chr, str)
  
  sapply(g, function(x) {sum(x != -1)})
}

# csvTextToDataFrame -----------------------------------------------------------

#' CSV Text to Data Frame
#' 
#' @param text character vector representing lines of comma separated values
#' @param \dots arguments passed to \code{\link[utils]{read.table}}
#' @export
#' 
csvTextToDataFrame <- function(text, ...)
{
  utils::read.table(text = text, ...)
}

# stringList -------------------------------------------------------------------

#' String of Comma Separated Quoted Strings
#' 
#' Create a string of comma separated quoted strings
#' 
#' @param x vector of character
#' @param qchar character to be used for quoting, default: single quote
#'   character
#' @param collapse characters used to separate the strings. Default: ", "
#' @export
#' 
stringList <- function(x, qchar = "'", collapse = ", ") 
{
  collapsed(hsQuoteChr(x, qchar = qchar), collapse = collapse)
}

# commaCollapsed ---------------------------------------------------------------

#' Paste With Collapse = ","
#' 
#' @param x vector of character
#' @export
#' 
commaCollapsed <- function(x)
{
  collapsed(x, ",")
}

# collapsed --------------------------------------------------------------------

#' Shortcut to paste(x, collapse = collapse)
#' 
#' This is just a shortcut to paste(x, collapse = collapse)
#'
#' @param x vector of character
#' @param collapse character string to separate the elements in x (passed to 
#'   \code{\link[base]{paste}})
#' @export
#' 
collapsed <- function(x, collapse = " ")
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
#' @export
#' 
hsQuoteChr <- function(
  x, qchar = "'", escapeMethod = c("double", "backslash", "none")
) 
{
  if (mode(x) == "list") {
    
    len <- length(x)
    
    if (len == 0) return (NULL)
    
    if (len == 1) return (hsQuoteChr(x[[1]]))
    
    return (c(hsQuoteChr(x[[1]]), hsQuoteChr(x[-1])))
  }
  
  escapeChars <- list(double = qchar, backslash = '\\', none = "")
  
  if (mode(x) == "character") {
    
    escapeChar <- escapeChars[[escapeMethod[1]]]
    
    x.escaped <- gsub(qchar, paste0(escapeChar, qchar), x)
    
    return (paste0(qchar, x.escaped, qchar))
  }
  
  x
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
#' @export
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
          paste0(
            "In the following strings the parts matching the pattern ", 
            "'%s' are replaced with '%s':\n  %s\n"
          ), 
          pattern, replacement, collapsed(items, ",\n  ")
        ))
      }
    }
  }
  
  strings
}

# hsTrim -----------------------------------------------------------------------

#' Remove Leading and Trailing Spaces
#' 
#' Remove leading, trailing (and, if requested, duplicate) spaces
#' 
#' @param str vector of character containing the strings to be trimmed
#' @param trim.multiple.spaces if TRUE (default), multiple consecutive spaces
#'   are replaced by one space
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) debut messages are
#'   shown
#' @return input string \emph{str} without leading or trailing spaces and with
#'   multiple consecutive spaces being replaced by a single space
#' @export
#' 
hsTrim <- function(str, trim.multiple.spaces = TRUE, dbg = FALSE) 
{
  replacements <- list("^\\s+" = "", "\\s+$" = "")
  
  if (trim.multiple.spaces) {
    
    replacements <- c(replacements, "\\s+" = " ")
  }
  
  multiSubstitute(str, replacements, dbg = dbg)
}

# removeSpaces -----------------------------------------------------------------

#' Remove all Spaces in String(s)
#' 
#' @param x (vector of) character
#' @return \emph{x} with all spaces removed
#' @export
#' 
removeSpaces <- function(x)
{
  gsub("\\s+", "", x)
}

# hsSubstSpecChars -------------------------------------------------------------

#' Deprecated. Use \code{\link{substSpecialChars}} instead.
#' 
#' @param x string containing special characters to be substituted
#' @return input string \emph{x} with special characters being substituted by 
#'   a meaningful represenation or underscore, multiple underscores replaced
#'   by a single underscore and multiple underscores at the end removed.
#' @export
#' 
hsSubstSpecChars <- function(x)
{
  warningDeprecated("hsSubstSpecChars", "substSpecialChars")
  
  substSpecialChars(x)
}

# substSpecialChars ------------------------------------------------------------

#' Substitution of Special Characters
#' 
#' @param x string containing special characters to be substituted
#' @param deuOnly logical. If \code{TRUE} only the German special characters 
#'   Umlaute and Eszett are replaced.
#' @return input string \emph{x} with special characters being substituted by 
#'   a meaningful represenation or underscore, multiple underscores replaced
#'   by a single underscore and multiple underscores at the end removed.
#' @export
#' 
substSpecialChars <- function(x, deuOnly = FALSE)
{
  replacements_deu <- list(
    "\\xc4" = "Ae", 
    "\\xe4" = "ae", 
    "\\xd6" = "Oe", 
    "\\xf6" = "oe", 
    "\\xdc" = "Ue", 
    "\\xfc" = "ue", 
    "\\xdf" = "ss"
  )
  
  replacements_grc <- list(
    "\\xb5" = "my"
  )
  
  replacements_other <- list(
    "%" = "proz",
    "\\\\" = "_",        # Replace backslash with underscore
    "[() -/.,;:?]" = "_", # Replace special characters with underscore
    "\\[" = "_",         # Replace opening bracket with underscore
    "\\]" = "_",         # Replace closing bracket with underscore
    "_+" = "_",          # Replace multiple underscores by one underscore
    "_$" = ""            # Remove underscore at the end
  )

  multiSubstitute(x, replacements = c(
    replacements_deu, 
    if (! deuOnly) {
      c(replacements_grc, replacements_other)
    } # else NULL
  ))
}

# stringToExpression -----------------------------------------------------------

#' Convert String to Expression
#' 
#' @param expressionString character string to be converted to an expression
#' @export
#' 
stringToExpression <- function(expressionString)
{
  parse(text = expressionString)
}

# stringContains ---------------------------------------------------------------

#' stringContains
#' 
#' @param x vector of character
#' @param contains vector of character
#' @export
#' @examples 
#' stringContains(c("abc", "Kabeljau", "Arabella"), "ab")
#' stringContains(c("abc", "Kabeljau", "Arabella"), "abc")
#'   
stringContains <- function(x, contains)
{
  grepl(contains, x)
}

# stringStartsWith -------------------------------------------------------------

#' stringStartsWith
#' 
#' @param x vector of character to be checked if they start with
#'   \code{startsWith}
#' @param startsWith string to be searched for at the beginning of the string(s)
#'   in \code{x}
#' @export
#' @examples 
#' stringStartsWith(c("abc", "Kabeljau", "Arabella"), "ab")
#' stringStartsWith(c("abc", "Kabeljau", "Arabella"), "A")
#'   
stringStartsWith <- function(x, startsWith)
{
  grepl(paste0("^", startsWith), x)
}

# stringEndsWith ---------------------------------------------------------------

#' stringEndsWith
#' 
#' @param x vector of character to be checked if they end with \code{endsWith}
#' @param endsWith string to be searched for at the end of the string(s) in
#'   \code{x}
#' @export
#' @examples 
#' stringEndsWith(c("abc", "Kabeljau", "Arabella"), "a")
#' stringEndsWith(c("abc", "Kabeljau", "Arabella"), "jau")
#'   
stringEndsWith <- function(x, endsWith)
{
  grepl(paste0(endsWith, "$"), x)
}

# extractSubstring -------------------------------------------------------------

#' Extract Substrings Defined by Regular Expressions
#' 
#' Extract substrings defined by regular expressions from a vector of strings
#' 
#' @param pattern regular expression containing parts in pairs of opening and
#'   closing parentheses defining the part(s) to be extracted
#' @param x vector of character strings
#' @param index index(es) of parenthesized subexpression(s) to be extracted. If
#'   the length of \code{x} is greater than one a data frame is returned with
#'   each column containing the substrings matching the subexpression at the
#'   corresponding index. If \code{index} is named, the names will be used as
#'   column names.
#' @param stringsAsFactors if \code{TRUE} (default is {FALSE}) and a data frame
#'   is returned then the columns in the returned data frame are of factors,
#'   otherwise vectors of character.
#' @export
#' @examples 
#' # Define pattern matching a date
#' pattern <- "([^ ]+), ([0-9]+) of ([^ ]+)"
#' 
#' # Extract single sub expressions from one string
#' datestring <- "Thursday, 8 of December"
#' extractSubstring(pattern, datestring, 1) # ""Thursday""
#' extractSubstring(pattern, datestring, 2) # "8"
#' extractSubstring(pattern, datestring, 3) # "December"
#' 
#' # Extract single sub expressions from a vector of strings
#' datestrings <- c("Thursday, 8 of December", "Tuesday, 14 of January")
#' extractSubstring(pattern, datestrings, 1) # "Thursday" "Tuesday"
#' extractSubstring(pattern, datestrings, 2) # "8"  "14"
#' extractSubstring(pattern, datestrings, 3) # "December" "January" 
#' 
#' # Extract more than one subexpression at once -> data.frame
#' extractSubstring(pattern, datestrings, 1:3)
#' 
#' #   subexp.1 subexp.2 subexp.3
#' #   1 Thursday        8 December
#' #   2  Tuesday       14  January
#' 
#' # Name the sub expressions by naming their number in index (3rd argument)
#' extractSubstring(pattern, datestrings, index = c(weekday = 1, 2, month = 3))
#' #    weekday subexp.2    month
#' # 1 Thursday        8 December
#' # 2  Tuesday       14  January
#' 
extractSubstring <- function(pattern, x, index, stringsAsFactors = FALSE)
{
  parts <- subExpressionMatches(pattern, x, simplify = FALSE)
  
  extract <- function(i) {
    sapply(parts, function(p) defaultIfNULL(p[[i]], ""))
  }

  if (length(index) > 1L) {
    
    # Make sure that all elements in index are named
    index <- nameElements(index, prefix = "subexp.")

    # Call extract() for each element in index
    args <- lapply(index, extract)

    # Arrange the result vectors in a data frame    
    do.call(data.frame, c(args, list(stringsAsFactors = stringsAsFactors)))
    
  } else {
    
    extract(index)
  }
}

# subExpressionMatches ---------------------------------------------------------

#' Find and Extract Regular Expressions from Strings
#' 
#' @param pattern regular expression containing parts in parentheses
#'   that are to be extracted from \emph{text}
#' @param text text to be matched against the regular expression
#' @param match.names optional. Names that are to be given to the extracted
#'   parts in the result list.
#' @param select named vector of numbers specifying the subexpressions in
#'   parentheses to be extracted.
#' @param simplify if TRUE (default) and \emph{text} has only one element, the
#'   output structure will be a list instead a list of lists
#' @param regularExpression deprecated. Use new argument \code{pattern} instead
#' @return If \code{length(text) > 1} a list is returned with as many elements
#'   as there are strings in \emph{text} each of which is itself a list
#'   containing the strings matching the subpatterns (enclosed in parentheses in
#'   \emph{pattern}) or NULL for strings that did not match. If
#'   \emph{match.names} are given, the elements of these lists are named
#'   according to the names given in \emph{match.names}. If \emph{text} is of
#'   length 1 and \emph{simplify} = TRUE (default) the top level list structure
#'   described above is omitted, i.e. the list of substrings matching the
#'   subpatterns is returned.
#' @export
#' @examples 
#' # split date into year, month and day
#' subExpressionMatches("(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23")
#' 
#' # split date into year, month and day (give names to the resulting elements)
#' x <- subExpressionMatches(
#'   pattern = "(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23",
#'   match.names = c("year", "month", "day")
#' )
#' 
#' cat(paste0("Today is ", x$day, "/", x$month, " of ", x$year, "\n"))
#' 
subExpressionMatches <- function(
  pattern, 
  text, 
  match.names = NULL,
  select = stats::setNames(seq_along(match.names), match.names),
  simplify = TRUE,
  regularExpression = NULL
)
{
  if ("regularExpression" %in% names(sys.call()[-1L])) {
    stop(
      "The argument 'regularExpression' is deprecated in ", 
      "subExpressionMatches(). Please use the new argument 'pattern' instead.", 
      call. = FALSE
    )
  }
  
  matchInfos <- regexec(pattern, text)
  
  result <- lapply(regmatches(text, matchInfos), function(x) {
    
    if (length(x)) {
      
      x <- as.list(x[-1L])
      
      # If numbers of subexpressions to select are given, select and name the
      # corresponding subexpressions
      if (length(select)) {
        x <- x[select]
      }
      
      stats::setNames(x, if (length(select)) names(select) else match.names)
    }
  })

  if (simplify && length(result) == 1L) {
    return(result[[1L]])
  }
  
  result
}

# nameElements -----------------------------------------------------------------
nameElements <- function(
  x, defaults = paste0(prefix, seq_along(x)), prefix = "x"
)
{
  elementNames <- defaultIfNULL(names(x), rep("", length(x)))
  
  isEmpty <- elementNames == ""
  
  elementNames[isEmpty] <- defaults[isEmpty]
  
  stats::setNames(x, elementNames)
}

# writeDictionary --------------------------------------------------------------

#' Write a Dictionary (List) to a Text File
#' 
#' @param dictionary list of character vectors of length one defining 
#' \code{key = value} pairs forming a dictionary.
#' @param file full path to the file to which \code{dictionary} is to be written
#' @seealso \code{\link{readDictionary}, \link{readDictionaries}}
writeDictionary <- function(dictionary, file)
{
  header_lines <- c(
    "This file defines a dictionary of paths where each left-hand-side (LHS)", 
    "expression can be used as a shortcut to the right-hand-side (RHS)",
    "expression. If a LHS appears in angle brackets within the RHS expression",
    "as the expression 'where' in '<where>/hand/side/expression' it is", 
    "replaced with the RHS expression that can be found for the LHS expression",
    "'where' (as in 'where = left') if there is such an assignment.",
    "Use kwb.utils::readDictionary() to read this dictionary from this file", 
    "into an R list and use kwb.utils::resolve() to resolve the expressions", 
    "in angle brackets (placeholders) in the RHS expressions."
  )
  
  header_lines <- c(
    header_lines,
    "This file has been generated using kwb.utils::writeDictionary()", 
    sprintf("on %s by user %s", Sys.Date(), kwb.utils::user())
  )
  
  header_lines <- paste("#", header_lines)
  body_lines <- paste(names(dictionary), "=", as.character(dictionary))
  
  text_lines <- c(header_lines, "", body_lines)
  
  writeLines(text_lines, con = file)
}

# readDictionaries -------------------------------------------------------------

#' Read Dictionary Files into a List of Dictionaries
#' 
#' Read files from a folder, specified by a file name pattern, into a list
#' of dictionaries
#' 
#' @param folder path to the folder containing the files to be read
#' @param pattern regular expression to match the names of the files to be read.
#' The pattern is expected to contain a pair of parentheses around the part
#' of the file that shall be used as element name in the returned list
#' @seealso \code{\link{readDictionary}}
readDictionaries <- function(folder, pattern = "^dictionary_(.*)[.]txt$")
{
  files <- dir(folder, pattern, full.names = TRUE)
  
  dictionaries <- lapply(files, kwb.utils::readDictionary, sorted = FALSE)
  
  config_names <- kwb.utils::subExpressionMatches(pattern, basename(files))
  
  structure(dictionaries, names = sapply(config_names, "[[", 1))
}

# readDictionary ---------------------------------------------------------------

#' Read Dictionary from Text File
#' 
#' Reads a dictionary (a list of "key = value"-pairs) from a text file.
#' 
#' @param file full path to dictionary file
#' @param sorted if TRUE (default) the entries in the dictionary will be sorted
#'   by their keys
#' 
#' @examples 
#'   file <- system.file("extdata", "dictionary.txt", package = "kwb.utils")
#'   
#'   dictionary <- readDictionary(file)
#'   
#'   resolve("file.out", dictionary, extension = "csv")
#'   resolve("file.out", dictionary, extension = "pdf")
#'   
#' @seealso \code{\link{readDictionaries}}
#' 
readDictionary <- function(file, sorted = TRUE)
{
  # Read the lines of the text file
  content <- readLines(safePath(file))
  
  # Trim all lines
  content <- unlist(lapply(content, hsTrim))
  
  # Remove empty rows and comment lines
  content <- content[content != "" & ! grepl("^\\s*#", content)]
  
  # Split the lines at the first equal sign
  key_value_pairs <- subExpressionMatches(
    "^([^= ]+)\\s*=\\s*(.*)$", content, match.names = c("key", "value")
  )

  # Create the dictionary
  dictionary <- toLookupList(
    keys = sapply(key_value_pairs, selectElements, "key"),
    values = sapply(key_value_pairs, selectElements, "value")  
  )

  if (sorted) {
    dictionary[order(names(dictionary))]
  } else {
    dictionary
  }
}

# resolve ----------------------------------------------------------------------

#' Resolve string(s) using a dictionary
#' 
#' Resolve string(s) using a dictionary
#' 
#' @param x vector of character to be resolved or a list of which all elements
#'   will be resolved using itself as a "dictionary". A dictionary is a list of 
#'   \code{key = value} pairs defining string replacements.
#' @param \dots Unnamed arguments are treated as (further) dictionaries. These
#'   are merged first to one dictionary before merging further (named) \code{key
#'   = value} pairs.
#' 
#' @examples 
#'   
#'   file <- system.file("extdata", "dictionary.txt", package = "kwb.utils")
#'   
#'   dictionary <- readDictionary(file)
#'   
#'   # Resolve the dictionary
#'   resolve(dictionary)
#'   
#'   # Resolve the dictionary by setting an undefined placeholder
#'   resolve(dictionary, extension = "pdf")
#'     
#'   # Resolve a string
#'   resolve("dir.project", dictionary)
#'   
#'   # Set a placeholder "on-the-fly"
#'   resolve("file.out", dictionary, extension = "pdf")
#'   
#'   # Override a placeholder "on-the-fly"
#'   resolve("dir.project", dictionary, project = "new_project")
#'   
#'   # Resolve a vector of strings
#'   resolve(c("dir.root", "dir.project"), dictionary, project = "vector")
#'
resolve <- function(x, ...)
{
  if (is.list(x)) {
    
    resolveAll(x, ...)
    
  } else {
    
    hsResolve(x, ...)
  }
}

# resolveAll -------------------------------------------------------------------

#' Resolve all Placeholders in a Dictionary
#' 
#' Resolve all placeholders in a dictionary
#' 
#' @param dictionary list with named elements where the element name represents
#'   the key and the element value represents the value assigned to the key.
#' @param \dots additional assignments of the form <key> = <value> that are
#'   temporarily added to the \code{dictionary} before doing the resolving
#' 
#' @examples 
#'   # Define a dictionary in the form of a list
#'   dictionary <- list(
#'     basedir = "C:/myNicefolder",
#'     projectdir = "<basedir>/projects/<project_name>",
#'     inputdir = "<projectdir>/input",
#'     outputdir = "<projectdir>/output"
#'   )
#'   
#'   # Resolve all entries in the dictionary, with different values for the
#'   # placeholder "<project_name> which is undefined in the original dictionary
#'   dictionary.1 <- resolveAll(dictionary, project_name = "project_1")
#'   dictionary.2 <- resolveAll(dictionary, project_name = "project_2")
#'   
#'   # Define entries of the dictionary to resolve
#'   keys <- c("inputdir", "outputdir")
#'   
#'   # Resolve the entries using the two different dictionaries
#'   resolve(keys, dictionary.1)
#'   resolve(keys, dictionary.2)
#'
resolveAll <- function(dictionary, ...)
{
  stopifnot(is.list(dictionary))
  
  lapply(dictionary, hsResolve, dictionary, ...)
}

# hsResolve --------------------------------------------------------------------

#' Resolve Placeholders in Dictionary
#' 
#' Resolve strings according to substitutions defined in dictionary
#' 
#' @param x (vector of) string expression(s) to be resolved using the dictionary
#'   \code{dict}.
#' @param dict dictionary: list with named elements where the element name
#'   represents the key and the element value represents the value assigned to
#'   the key.
#' @param \dots additional named arguments that are added to \code{dict} before
#'   resolving
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) debug messages are 
#'   shown
hsResolve <- function(x, dict = NULL, ..., dbg = FALSE)
{
  # Apply hsResolve to each element if more than one element in x 
  if (length(x) > 1) {
    
    return(sapply(x, hsResolve, dict = dict, ..., dbg = dbg, USE.NAMES = FALSE))
  }
  
  catIf(dbg, sprintf("Resolving \"%s\"...\n", x))
  
  # Add named arguments to the dictionary
  assignments <- list(...)

  # If there are unnamed elements in the assignments treat them as further
  # dictionaries
  isUnnamed <- is.unnamed(assignments)
  
  if (any(isUnnamed)) {
    
    if (! all(sapply(assignments[isUnnamed], is.list))) {
      
      stop("All unnamed arguments given in ... are expected to be lists!")
    }
    
    dict <- arglist(dict, do.call(arglist, assignments[isUnnamed]))
    
    assignments <- assignments[! isUnnamed]
  }
  
  # Mix (override existing entries or add new entries) the existing assignments
  # with the (remaining) named assignments
  dict <- arglist(dict, assignments)
  
  ## Lookup the key in the dictionary
  value <- as.character(dict[[x]]) ## just in case: convert factor to string
  
  ## If there was a value found for the key, resolve the value and not the key
  if (length(value) > 0) {
    
    catIf(dbg, sprintf("Value of key '%s': '%s'\n", x, value))
    
    return (hsResolve(value, dict = dict, dbg = dbg))    
  }
  
  ## Here, the value is NULL so let's continue with the key x
  value <- x
  
  ## Find <tags> to resolve
  tags <- getTagNames(value, expected.length = 1)[[1]]
  
  # Return the value if there are no tags to be resolved
  if (length(tags) == 0) {
    
    catIf(dbg, sprintf("\"%s\" resolved to itself: \"%s\".\n", value, value))
    
    return (x)
  }
  else {
    
    printIf(dbg, tags)
  }  
  
  ## Resolve the tags
  resolvedTags <- hsResolve(tags, dict = dict, dbg = dbg)
  
  ## For the tags that could be resolved substitute each occurrence of the tag 
  ## with its resolved value
  toResolve <- (resolvedTags != tags)
  
  if (any(toResolve)) {
    
    keys <- paste0("<", tags[toResolve], ">")
    replacements <- toLookupList(keys = keys, values = resolvedTags[toResolve])
    value <- multiSubstitute(value, replacements, fixed = TRUE)
  }
  
  catIf(dbg, sprintf("\"%s\" resolved to: \"%s\".\n", x, value))
  
  # Return the (resolved) value
  value
}

# getTagNames ------------------------------------------------------------------

#' Find <tag>-tags in string
#' 
#' Return tags of the form <tag> that are contained in the string \emph{x}.
#' 
#' @param x vector of character
#' @param bt bracket type, must be one of c("<>", "[]"). Default: "<>"
#' @param dbg if \code{TRUE} (default is \code{FALSE}) debug messages are shown
#' @param expected.length if given and different from the length of \code{x}
#'   an error is thrown
#' 
getTagNames <- function(
  x, bt = c("<>", "[]")[1], dbg = FALSE, expected.length = length(x)
) 
{
  if (! is.character(x)) {
    stop("x must be a vector of character")
  }
  
  if (length(x) != expected.length) {
    stop("x must be of length ", expected.length)
  }
  
  ## Set pattern according to bracket type
  patterns <- list(
    "<>" = "<([^<>]+)>",
    "[]" = "\\[([^][]*)\\]"
  )
  
  p <- patterns[[bt]]
  
  if (is.null(p)) {
    stop("bt (bracket type) must be one of c(\"<>\", \"[]\")!")
  }
  
  catIf(dbg, "pattern:", p, "\n")
  
  ## Get matching strings (including parentheses)
  matches <- regmatches(x, gregexpr(p, x))
  
  ## Remove parentheses
  lapply(matches, gsub, pattern = p, replacement = "\\1")
}

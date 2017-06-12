# readDictionary ---------------------------------------------------------------

#' read dictionary from text file
#' 
#' reads a dictionary (a list of "key = value"-pairs) from a text file.
#' 
#' @param file full path to dictionary file
#' @param sorted if TRUE (default) the entries in the dictionary will be sorted by their
#'   keys
#' 
#' @examples 
#'   file <- system.file("extdata", "dictionary.txt", package = "kwb.utils")
#'   
#'   dictionary <- readDictionary(file)
#'   
#'   resolve("file.out", dictionary, extension = "csv")
#'   resolve("file.out", dictionary, extension = "pdf")
#'   
#' 
readDictionary <- structure(
  function # read dictionary from text file
### reads a dictionary (a list of "key = value"-pairs) from a text file.
(
  file,
  ### full path to dictionary file
  sorted = TRUE
  ### if TRUE (default) the entries in the dictionary will be sorted by their
  ### keys
)
{
  content <- utils::read.table(file, sep = "=", stringsAsFactors = FALSE)
       
  dictionary <- do.call(toLookupList, lapply(unname(content[, 1:2]), hsTrim))

  if (sorted) {
    dictionary[order(names(dictionary))]
  } else {
    dictionary
  }
}, ex = function() {
  file <- system.file("extdata", "dictionary.txt", package = "kwb.utils")
  
  dictionary <- readDictionary(file)
  
  resolve("file.out", dictionary, extension = "csv")
  resolve("file.out", dictionary, extension = "pdf")
})

# resolve ----------------------------------------------------------------------

#' Resolve string(s) using a dictionary
#' 
#' Resolve string(s) using a dictionary
#' 
#' @param x vector of character to be resolved or a list of which all elements will be
#'   resolved using itself as a "dictionary". A dictionary is a list of 
#'   \code{key = value} pairs defining string replacements.
#' @param \dots Unnamed arguments are treated as (further) dictionaries. These are merged
#'   first to one dictionary before merging further (named) \code{key = value} 
#'   pairs.
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
#' 
resolve <- structure(function # Resolve string(s) using a dictionary
### Resolve string(s) using a dictionary
(
  x,
  ### vector of character to be resolved or a list of which all elements will be
  ### resolved using itself as a "dictionary". A dictionary is a list of 
  ### \code{key = value} pairs defining string replacements.
  ...
  ### Unnamed arguments are treated as (further) dictionaries. These are merged
  ### first to one dictionary before merging further (named) \code{key = value} 
  ### pairs.
)
{
  if (is.list(x)) {
    
    resolveAll(x, ...)
  } 
  else {
    
    hsResolve(x, ...)
  }
}, ex = function() {
  
  file <- system.file("extdata", "dictionary.txt", package = "kwb.utils")
  
  dictionary <- readDictionary(file)
  
  # Resolve the dictionary
  resolve(dictionary)
  
  # Resolve the dictionary by setting an undefined placeholder
  resolve(dictionary, extension = "pdf")
    
  # Resolve a string
  resolve("dir.project", dictionary)

  # Set a placeholder "on-the-fly"
  resolve("file.out", dictionary, extension = "pdf")
  
  # Override a placeholder "on-the-fly"
  resolve("dir.project", dictionary, project = "new_project")
  
  # Resolve a vector of strings
  resolve(c("dir.root", "dir.project"), dictionary, project = "vector")
})

# resolveAll -------------------------------------------------------------------

#' resolve all placeholders in a dictionary
#' 
#' resolve all placeholders in a dictionary
#' 
#' @param dictionary list with named elements where the element name represents the key and
#'   the element value represents the value assigned to the key.
#' @param \dots additional assignments of the form <key> = <value> that are temporarily
#'   added to the \code{dictionary} before doing the resolving
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
#' 
resolveAll <- structure(
  function # resolve all placeholders in a dictionary
  ### resolve all placeholders in a dictionary
  (
    dictionary,
    ### list with named elements where the element name represents the key and
    ### the element value represents the value assigned to the key.
    ...
    ### additional assignments of the form <key> = <value> that are temporarily
    ### added to the \code{dictionary} before doing the resolving
  )
  {
    stopifnot(is.list(dictionary))
    
    lapply(dictionary, hsResolve, dictionary, ...)
  },
  ex = function() {
    # Define a dictionary in the form of a list
    dictionary <- list(
      basedir = "C:/myNicefolder",
      projectdir = "<basedir>/projects/<project_name>",
      inputdir = "<projectdir>/input",
      outputdir = "<projectdir>/output"
    )
    
    # Resolve all entries in the dictionary, with different values for the
    # placeholder "<project_name> which is undefined in the original dictionary
    dictionary.1 <- resolveAll(dictionary, project_name = "project_1")
    dictionary.2 <- resolveAll(dictionary, project_name = "project_2")
    
    # Define entries of the dictionary to resolve
    keys <- c("inputdir", "outputdir")
    
    # Resolve the entries using the two different dictionaries
    resolve(keys, dictionary.1)
    resolve(keys, dictionary.2)
  })

# hsResolve --------------------------------------------------------------------

#' Resolve Placeholders in Dictionary
#' 
#' Resolve strings according to substitutions defined in dictionary
#' 
#' @param x (vector of) string expression(s) to be resolved using the dictionary
#'   \code{dict}.
#' @param dict dictionary: list with named elements where the element name represents
#'   the key and the element value represents the value assigned to the key.
#' @param \dots additional named arguments that are added to \code{dict} before resolving
#' @param dbg if \code{TRUE} (the default is \code{FALSE}) debug messages are 
#'   shown
hsResolve <- function
(
  x, 
  dict = NULL, 
  ...,
  dbg = FALSE
) 
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
getTagNames <- function
(
  x,
  bt = c("<>", "[]")[1],
  dbg = FALSE,
  expected.length = length(x)
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

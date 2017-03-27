# readDictionaryFromFile -------------------------------------------------------
readDictionaryFromFile <- function # readDictionaryFromFile
### reads a dictionary (a list of "key = value"-pairs) from a text file.
(
  dictionaryFile,
  ### full path to dictionary file
  sorted = TRUE
  ### if TRUE (default) the entries in the dictionary will be sorted by their
  ### keys
)
{
  dictionaryTable <- read.table(dictionaryFile, sep = "=", 
                                stringsAsFactors=FALSE) 
  
  dictionary <- as.list(hsTrim(dictionaryTable$V2))
  names(dictionary) <- hsTrim(dictionaryTable$V1)
  
  if (sorted) {
    dictionary <- dictionary[order(names(dictionary))]
  }
  
  dictionary
}

# hsAddToDict ------------------------------------------------------------------
hsAddToDict <- function # hsAddToDict
### add assignements given in ... list to \emph{dictionary}
(
  dictionary = NULL, ...
) 
{
  ## Append additional arguments to dictionary
  c(dictionary, list(...))  
}

# resolveAll -------------------------------------------------------------------
resolveAll <- structure(
  function # resolve all placeholders in a dictionary
  ### resolve all placeholders in a dictionary
  (
    dictionary
    ### list with named elements where the element name represents the key and the
    ### element value represents the value assigned to the key.
  )
  {
    lapply(dictionary, hsResolve, dictionary)
  }, 
  ex = function() {
    dictionary <- list(
      basedir = "C:/myNicefolder",
      projectdir = "<basedir>/projects/<projectName>",
      inputdir = "<projectdir>/input",
      outputdir = "<projectdir>/output"
    )
    
    dictionary$projectName <- "project1"
    dictionary1 <- resolveAll(dictionary)

    dictionary$projectName <- "project2"
    dictionary2 <- resolveAll(dictionary)
    
    dictionary1$input
    dictionary1$output    
    
    dictionary2$inputdir
    dictionary2$output    
  })

# hsResolve --------------------------------------------------------------------
hsResolve <- function
### Resolve strings according to substitutions defined in dictionary
(
  x, 
  ### (vector of) string expression(s) to be resolved using the dictionary
  ### \emph{dict}.
  dict, 
  ### dictionary: list with named elements where the element name represents
  ### the key and the element value represents the value assigned to the key.  
  dbg = FALSE
) 
{ 
  #@2012-04-11;HSB;apply hsResolve to each element if more than one element in x 
  if (length(x) > 1) {
    return(sapply(x, hsResolve, dict = dict, dbg = dbg, USE.NAMES = FALSE))
  }
  
  if (dbg) 
    cat(sprintf("Resolving \"%s\"...\n", x))
  
  ## Lookup the key in the dictionary
  val <- as.character(dict[[x]]) ## just in case: convert Factor to string
  
  ## If there was a value found for the key, resolve the value and not the key
  if (length(val) > 0) { 
    if (dbg) 
      cat(sprintf("Value of key '%s': '%s'\n", x, val))
    return(hsResolve(val, dict = dict, dbg = dbg))    
  }
  
  ## Here, the value is NULL so let's continue with the key x
  val <- x
  
  ## Find <tags> to resolve
  tags <- hsTags(val, bt = "<>")
  
  # Return the value if there are no tags to be resolved
  if (length(tags) == 0) {
    if (dbg) 
      cat(sprintf("\"%s\" resolved to itself: \"%s\".\n", val, val))
    return(x)    
  }
  else if (dbg) {
    cat("tags:\n")
    print(tags)
  }  
  
  ## Resolve the tags
  rTags <- hsResolve(tags, dict = dict, dbg = dbg)
  
  ## For the tags that could be resolved substitute each occurrence of the tag 
  ## with its resolved value
  for (i in (1:length(tags))[rTags != tags]) {
    val <- gsub(sprintf("<%s>", tags[i]), rTags[i], val)    
  }
  
  if (dbg) 
    cat(sprintf("\"%s\" resolved to: \"%s\".\n", x, val))
  
  # Return the (resolved) value
  return(val)
}

# hsTags -----------------------------------------------------------------------
hsTags <- function # Find <tag>-tags in string
### Return tags of the form <tag> that are contained in the string \emph{x}.
(
  x, 
  bt, 
  ### bracket type, must be one of c("<>", "[]")
  dbg = FALSE
) 
{
  # x must be of length 1
  if (length(x) != 1) 
    stop("x must be of length 1.\n")
  
  hsTags2(x, bt = bt, dbg = dbg)[[1]]
}

# hsTags2 ----------------------------------------------------------------------
hsTags2 <- function # Find <tag>-tags in string
### Return tags of the form <tag> that are contained in the string \emph{x}.
(
  x, 
  bt,
  ### bracket type, must be one of c("<>", "[]")
  dbg = FALSE
) 
{
  ## Set pattern according to bracket type
  if (bt == "<>") {
    p <- "<([^<>]+)>"
  }
  else if (bt == "[]") {
    p <- "\\[([^][]*)\\]"
  }
  else {
    stop("bt (bracket type) must be one of c(\"<>\", \"[]\")!")
  }
  
  if (dbg)
    cat("pattern:", p, "\n")
  
  ## Get matching strings (including parentheses)
  m <- regmatches(x, gregexpr(p, x))
  
  ## Remove parentheses
  lapply(m, gsub, pattern = p, replacement = "\\1")
}

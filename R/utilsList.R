# selectElements ---------------------------------------------------------------
selectElements <- structure(
  function # select (and rename) elements from list
### select (and rename, if required) elements from list. Stop with message if
### elements do not exist
(
  x, 
  ### list
  elements,
  ### vector of element names. The names of named elements will be the names
  ### in the output list
  do.stop = TRUE,
  ### this flag controls whether the function stops (\code{do.stop = TRUE}) or
  ### not (\code{do.stop = FALSE}) if there are non-existing elements to be
  ### selected. If \code{do.stop = FALSE} only those elements are selected that
  ### actually exist
  do.warn = TRUE
  ### if \code{TRUE} (default) and \code{do.stop = FALSE} a warning is given if
  ### elements do not exist. Set to \code{FALSE} to suppress warnings
)
{
  listName <- deparse(substitute(x))
  
  if (! is.list(x)) {
    stop(listName, " is not a list but:\n", 
         paste(capture.output(str(x)), collapse = "\n"))
  }
  
  availableNames <- names(x)
  
  missingElements <- elements[! elements %in% availableNames]
  
  if (length(missingElements) > 0) {
    output <- paste0(
      "No such list elements: ", commaCollapsed(missingElements), "\n",
      "Available elements: ", commaCollapsed(availableNames)
    )
    if (isTRUE(do.stop)) {
      stop(output)
    } else {
      if (isTRUE(do.warn)) {
        warning(paste0(output, "\nOnly the existing elements are selected."))
      }
      elements <- intersect(elements, availableNames)
    }
  }
  
  L <- length(elements) 
  
  if (L == 0) {
    result <- list()
  } else if (L == 1) {
    result <- x[[elements]]
  } else {
    result <- x[elements]
  }
  
  newNames <- names(elements)
  
  if (! is.null(newNames)) {
    is.given <- newNames != ""
    names(result)[is.given] <- newNames[is.given]
  }  
  
  result  
  ### list containing the elements of \code{x} that are specified in 
  ### \code{elements} or \code{x[[elements]]} if length of \code{elements} is 1 
  ### or \code{list()} if \code{elements} is empty. If the elements in vector 
  ### \code{elements} are named, these names are used in the output list.
}, ex = function() {
  L <- list(a = 1, b = 2, c = 3, d = 4)
  
  # Select elements
  selectElements(L, c("a", "c"))
  
  # Select and rename at the same time
  selectElements(L, elements = c(a.new = "a", c.new = "c", "b"))
})

# getElementLengths ------------------------------------------------------------

#' Get the Lenghts of List Elements
#'
#' @param x a list
#' @return vector of integer
#' @export
#' @examples 
#' x <- list(a = 1:3, b = list(x = 1, y = 2), c = 1:100)
#' getElementLengths(x)
#' 
getElementLengths <- function(x)
{
  if (! is.list(x)) {
    
    stop(call. = FALSE, sprintf(
      "The object '%s' given to getElementLengths() is not a list but: '%s'",
      deparse(substitute(x)), mode(x)
    ))
  }
  
  if (length(x) == 0) {
    
    integer()
    
  } else {
    
    sapply(x, length)
  }
}

# getPathsAndValuesFromRecursiveList -------------------------------------------

#' Get Paths and String Values from Recursive List
#' 
#' @param x a list
#' @param path start path
#' @return data frame with columns \code{path} and \code{value}. The 
#' data frame contains all non-list elements that are contained in \code{x}, 
#' coerced to character, in column \code{value}, together with the sequence of
#' element names "leading" to the value when starting at \code{x}. For example,
#' the path to element \code{x$a$a1} is \code{/a/a1} (see example).
#' @export
#' @examples
#' # Define a recursive list
#' x <- list(
#'   a = list(a1 = "A1", a2 = "A2"),
#'   b = list(b1 = "B1", b2 = "B2", b3 = "B3"),
#'   c = list(c1 = list(c11 = "C11"), c2 = list(c21 = "C21", c22 = "C22"))
#' )
#' 
#' # Get all non-list-elements and their "path" as a data frame
#' getPathsAndValuesFromRecursiveList(x)
#' 
getPathsAndValuesFromRecursiveList <- function(x, path = "")
{
  if (is.list(x)) {
    
    elements <- names(x)
    
    stopifnot(length(elements) == length(x) && all(elements != ""))
    
    do.call(rbind, lapply(elements, function(element) {
      
      getPathsAndValuesFromRecursiveList(x[[element]], if (path == "") {
        element 
      } else {
        file.path(path, element)
      })
      
    }))
    
  } else {
    
    data.frame(path = path, value = as.character(x), stringsAsFactors = FALSE)
  }
}

# copyListElements -------------------------------------------------------------

#' Copy List Elements into a List of Lists
#'
#' @param x list of lists
#' @param y list of elements
#' @param name name of target list element
#' @return \code{x} with each sublist being extended by list element \code{name}
#'   having been taken from \code{y}
#' @export
#' @examples 
#' x <- list(list(a = 1), list(b = 2), list(c = 3))
#' y <- list("b1", "b2", "b3")
#' str(copyListElements(x, y, "b"))
#' 
copyListElements <- function(x, y, name = deparse(substitute(y))) {
  
  stopifnot(is.list(x), is.list(y), is.character((name)))
  
  stopifnot(all(sapply(x, is.list)))
  
  stopifnot(length(x) == length(y))
  
  stopifnot(length(name) == 1)

  lapply(seq_along(x), function(i) {
    
    result <- x[[i]]
    
    result[name] <- y[i]
    
    result
  })
}

# excludeNULL ------------------------------------------------------------------

#' Exclude all NULL Entries from a List
#' 
#' @param x a list
#' @param dbg if \code{TRUE} (default) a message is shown if elements are 
#'   removed
#' @return list \code{x} with all \code{NULL} entries excluded
#' @export
#' @examples 
#' L <- list(a = 1, b = NULL, c = "three")
#' L
#' 
#' excludeNULL(L)
#'
excludeNULL <- function(x, dbg = TRUE)
{
  stopifnot(is.list(x))
 
  is_null <- sapply(x, is.null)
  
  if (any(is_null)) {
    
    x <- catAndRun(
      dbg = dbg, 
      sprintf("Removing %d list elements that are NULL", sum(is_null)), 
      x[! is_null]
    )
  }
  
  x
}

# toNamedList ------------------------------------------------------------------

#' Convert to List with Names Equal to List Elements
#' 
#' @param x R object, preferably vector of character
#' @return result of calling \code{as.list} on \code{x} with list element names
#'   being set to the elements of \code{x}.
#' @export
#' @examples
#' x.vector <- c("Peter", "Paul", "Mary")
#' x.list <- toNamedList(x.vector)
#' all(names(x.list) == x.vector)
#' 
#' # You may use toNamedList in a call of lapply in order to get a named result
#' lapply(toNamedList(x.vector), function(x) sprintf("Hello, %s", x))
#' 
#' # Compare with
#' lapply(x.vector, function(x) sprintf("Hello, %s", x))
#' 
toNamedList <- function(x)
{
  x <- as.character(x)
  
  structure(as.list(x), names = x)
}

# nameByElement ----------------------------------------------------------------

#' Name List Elements by Sublist Element
#' 
#' name the elements of a list of lists by the value of the element 
#' \code{elementName} of each sublist
#' 
#' @param x list of lists
#' @param elementName name of element to be looked up in each sublist of
#'   \code{x}
#' @export
#' @examples 
#' L <- list(
#'   list(group = "A", value = 1),
#'   list(group = "B", value = 2)
#' )
#' 
#' nameByElement(L, "group")
#'
nameByElement <- function(x, elementName = "name")
{
  structure(x, names = sapply(x, selectElements, elementName))
}
  
# selectElements ---------------------------------------------------------------

#' Select (and Rename) Elements from List
#' 
#' select (and rename, if required) elements from list. Stop with message if 
#' elements do not exist
#' 
#' @param x list
#' @param elements vector of element names. The names of named elements will be the names
#'   in the output list
#' @param do.stop this flag controls whether the function stops (\code{do.stop = TRUE}) or
#'   not (\code{do.stop = FALSE}) if there are non-existing elements to be
#'   selected. If \code{do.stop = FALSE} only those elements are selected that
#'   actually exist
#' @param do.warn if \code{TRUE} (default) and \code{do.stop = FALSE} a warning is given if
#'   elements do not exist. Set to \code{FALSE} to suppress warnings
#' @return list containing the elements of \code{x} that are specified in 
#'   \code{elements} or \code{x[[elements]]} if length of \code{elements} is 1 
#'   or \code{list()} if \code{elements} is empty. If the elements in vector 
#'   \code{elements} are named, these names are used in the output list.
#' @export
#' @examples 
#' L <- list(a = 1, b = 2, c = 3, d = 4)
#'   
#' # Select elements
#' selectElements(L, c("a", "c"))
#' 
#' # Select and rename at the same time
#' selectElements(L, elements = c(a.new = "a", c.new = "c", "b"))
#' 
selectElements <- function(x, elements = NULL, do.stop = TRUE, do.warn = TRUE)
{
  listName <- deparse(substitute(x))
  
  if (! is.list(x)) {
    stopIsNotBut(x, "list", listName)
  }
  
  available <- names(x)
  
  if (is.null(elements)) {
    stop(
      "No elements selected. ", 
      hintAvailable(available, sorted = TRUE), 
      call. = FALSE
    )
  }
  
  missingElements <- elements[! elements %in% available]
  
  if (length(missingElements) > 0) {
    
    msg <- noSuchElements(
      missingElements, 
      available, 
      type = "element", 
      suffix = sprintf(" in list '%s'", collapsed(deparse(substitute(x))))
    )
    
    if (isTRUE(do.stop)) {
      
      stop(msg, call. = FALSE)
      
    } else {
      
      if (isTRUE(do.warn)) {
        warning(
          msg, "\nOnly the existing elements are selected.", call. = FALSE
        )
      }
      
      elements <- intersect(elements, available)
    }
  }
  
  L <- length(elements) 
  
  if (L == 0L) {
    return(list())
  } 
  
  if (L == 1L) {
    return(x[[elements]])
  } 
  
  result <- x[elements]

  newNames <- names(elements)
  
  if (! is.null(newNames)) {
    
    is.given <- newNames != ""
    
    names(result)[is.given] <- newNames[is.given]
  }  
  
  result  
}

# removeElements ---------------------------------------------------------------

#' Remove Elements from a List
#' 
#' @param x a list
#' @param elements names of elements to remove
#' @return \code{x} with elements with names given in \code{elements} being
#'   removed. User attributes of \emph{x} are restored.
#' @export
#' 
removeElements <- function(x, elements)
{
  attributes.x <- attributes(x)
  
  out <- selectElements(x, setdiff(names(x), elements))
  
  hsRestoreAttributes(out, attributes.x)
}

# copyListElements -------------------------------------------------------------
#' Copy list elements into a list of lists
#'
#' @param x list of lists
#' @param y list of elements
#' @param name name of target list element
#' @return \code{x} with each sublist being extended by list element \code{name}
#' having been taken from \code{y}
#' @examples 
#' x <- list(list(a = 1), list(b = 2), list(c = 3))
#' y <- list("b1", "b2", "b3")
#' str(copyListElements(x, y, "b"))
copyListElements <- function(x, y, name) {
  
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

#' Exclude all NULL entries from a list
#' 
#' Exclude all null entries from a list
#' 
#' @param x a list
#' 
#' @return list \code{x} with all \code{NULL} entries excluded
#' 
#' @examples 
#'   L <- list(a = 1, b = NULL, c = "three")
#'   L
#'   
#'   excludeNULL(L)
#'   
#' 
excludeNULL <- structure(
  function # Exclude all NULL entries from a list
  ### Exclude all null entries from a list
  (
    x
    ### a list
  )
  {
    stopifnot(is.list(x))
    
    x[! sapply(x, is.null)]
    ### list \code{x} with all \code{NULL} entries excluded
  }, ex = function() {
    L <- list(a = 1, b = NULL, c = "three")
    L
    
    excludeNULL(L)
  })

# toNamedList ------------------------------------------------------------------

#' convert to list with names equal to list elements
#' 
#' convert to list with names equal to list elements
#' 
#' @param x R object, preferably vector of character
#' 
#' @return result of calling \code{as.list} on \code{x} with list element names being
#'   set to the elements of \code{x}.
#' 
#' @examples 
#'   
#'   x.vector <- c("Peter", "Paul", "Mary")
#'   x.list <- toNamedList(x.vector)
#'   all(names(x.list) == x.vector)
#'   
#'   # You may use toNamedList in a call of lapply in order to get a named result
#'   lapply(toNamedList(x.vector), function(x) sprintf("Hello, %s", x))
#'   
#'   # Compare with
#'   lapply(x.vector, function(x) sprintf("Hello, %s", x))
#'   
#' 
toNamedList <- structure(
  function # convert to list with names equal to list elements
  ### convert to list with names equal to list elements
  (
    x
    ### R object, preferably vector of character
  )
  {
    x <- as.character(x)
    
    structure(as.list(x), names = x)
    ### result of calling \code{as.list} on \code{x} with list element names being
    ### set to the elements of \code{x}.
  }, 
  ex = function() {
    
    x.vector <- c("Peter", "Paul", "Mary")
    x.list <- toNamedList(x.vector)
    all(names(x.list) == x.vector)
    
    # You may use toNamedList in a call of lapply in order to get a named result
    lapply(toNamedList(x.vector), function(x) sprintf("Hello, %s", x))
    
    # Compare with
    lapply(x.vector, function(x) sprintf("Hello, %s", x))
  }
)

# nameByElement ----------------------------------------------------------------

#' name list elements by sublist element
#' 
#' name the elements of a list of lists by the value of the element
#'   \code{elementName} of each sublist
#' 
#' @param x list of lists
#' @param elementName name of element to be looked up in each sublist of \code{x}
#' 
#' @examples 
#'   L <- list(
#'     list(group = "A", value = 1),
#'     list(group = "B", value = 2)
#'   )
#'   
#'   nameByElement(L, "group")
#'   
#' 
nameByElement <- structure(
  function # name list elements by sublist element
  ### name the elements of a list of lists by the value of the element
  ### \code{elementName} of each sublist
  (
    x, 
    ### list of lists
    elementName = "name"
    ### name of element to be looked up in each sublist of \code{x}
  )
  {
    structure(x, names = sapply(x, selectElements, elementName))
  }, ex = function() {
    L <- list(
      list(group = "A", value = 1),
      list(group = "B", value = 2)
    )
    
    nameByElement(L, "group")
  })

# selectElements ---------------------------------------------------------------

#' select (and rename) elements from list
#' 
#' select (and rename, if required) elements from list. Stop with message if
#'   elements do not exist
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
#' 
#' @return list containing the elements of \code{x} that are specified in 
#'   \code{elements} or \code{x[[elements]]} if length of \code{elements} is 1 
#'   or \code{list()} if \code{elements} is empty. If the elements in vector 
#'   \code{elements} are named, these names are used in the output list.
#' 
#' @examples 
#'   L <- list(a = 1, b = 2, c = 3, d = 4)
#'   
#'   # Select elements
#'   selectElements(L, c("a", "c"))
#'   
#'   # Select and rename at the same time
#'   selectElements(L, elements = c(a.new = "a", c.new = "c", "b"))
#'   
#' 
selectElements <- structure(
  function # select (and rename) elements from list
  ### select (and rename, if required) elements from list. Stop with message if
  ### elements do not exist
  (
    x, 
    ### list
    elements = NULL,
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
           paste(utils::capture.output(utils::str(x)), collapse = "\n"))
    }
    
    available <- names(x)
    
    messageAvailable <- paste0(
      "Available elements: ", stringList(sort(available))
    )
    
    if (is.null(elements)) {
      stop("No elements selected. ", messageAvailable)
    }
    
    missingElements <- elements[! elements %in% available]
    
    if (length(missingElements) > 0) {
      
      output <- paste0(
        "No such list elements: ", stringList(missingElements), "\n",
        messageAvailable
      )
      
      if (isTRUE(do.stop)) {
        stop(output)
      } else {
        if (isTRUE(do.warn)) {
          warning(paste0(output, "\nOnly the existing elements are selected."))
        }
        elements <- intersect(elements, available)
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

# removeElements ---------------------------------------------------------------

#' remove elements from a list
#' 
#' remove elements from a list
#' 
#' @param x a list
#' @param elements names of elements to remove
#' 
#' @return \code{x} with elements with names given in \code{elements} being removed.
#'   User attributes of \emph{x} are restored.
#' 
removeElements <- function # remove elements from a list
### remove elements from a list
(
  x,
  ### a list
  elements
  ### names of elements to remove
)
{
  attributes.x <- attributes(x)
  
  out <- selectElements(x, setdiff(names(x), elements))
  
  hsRestoreAttributes(out, attributes.x)
  ### \code{x} with elements with names given in \code{elements} being removed.
  ### User attributes of \emph{x} are restored.
}

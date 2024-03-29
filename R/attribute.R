# addClass ---------------------------------------------------------------------

#' Add a Class to an Object
#' 
#' Extend the class attribute by a given class name
#' 
#' @param x R object
#' @param className name of the class to be added to \code{x}
#' @param first if \code{TRUE} (default) the \code{className} is prepended to
#'   the vector of existing class names, otherwise appended to the end of the
#'   vector
#' @param dbg if \code{TRUE} (default), debug messages are shown.
#' @return \code{x} with the \code{class} attributed extended by
#'   \code{className} (only if \code{x} did not yet inherit from
#'   \code{className})
#' @export
#' 
addClass <- function(x, className, first = TRUE, dbg = FALSE)
{
  # Extend the class attribute if x does not yet inherit from the class of the
  # given name
  if (! inherits(x, className)) {
    
    catIf(
      dbg, "Adding class", hsQuoteChr(className), "to object",
      hsQuoteChr(deparse(substitute(x)))
    )
    
    class(x) <- if (isTRUE(first)) {
      
      c(className, class(x))
      
    } else {
      
      c(class(x), className)
    }
  }
  
  x
}

# hsRestoreAttributes ----------------------------------------------------------

#' Restore Object Attributes
#' 
#' Restore given attributes that are not object attributes any more
#' 
#' @param x object
#' @param attribs former attributes of x (as retrieved by attributes(x)) to be
#'   restored
#' @export
#' 
hsRestoreAttributes <- function(x, attribs)
{
  for (attrib in setdiff(names(attribs), names(attributes(x)))) {
    
    attr(x, attrib) <- attribs[[attrib]]
  }
  
  x
}

# getAttribute -----------------------------------------------------------------

#' Safely get the Attribute of an Object
#' 
#' Safely get the attribute of an object. An error is given if the attribute 
#' does not exist (and do.stop = TRUE)
#' 
#' @param x object from which to take the attribute
#' @param attributeName name of the attribute to be returned
#' @param do.stop if \code{TRUE} (default) an error is raised if the attribute
#'   does not exist.
#' @export
#' @examples 
#' 
#' x <- structure(1, a = 2)
#' # getAttribute(x, "b") # gives a clear error message
#' identical(getAttribute(x, "a"), attr(x, "a")) # is TRUE
#' 
#' # Get an attribute's attribute by means of a "path" notation
#' y <- structure(1, a = structure(2, b = 3))
#' z <- structure(4, y = y)
#' 
#' str(y)
#' str(z)
#' 
#' kwb.utils::getAttribute(y, "a/b")
#' kwb.utils::getAttribute(z, "y/a/b")
#' 
getAttribute <- function(x, attributeName, do.stop = TRUE)
{
  # If the attribute name contains slash(es), split the name at the slash 
  # character and call this function for each segment
  split_at <- "/"
  
  if (grepl(split_at, attributeName)) {
    
    segments <- strsplit(attributeName, "/")[[1L]]
    
    result <- x
    
    while (length(segments)) {
      result <- getAttribute(result, segments[1L], do.stop = do.stop)
      segments <- segments[-1L]
    }
    
    return (result)
  }
  
  attributeNames <- names(attributes(x))
  
  if (do.stop && ! attributeName %in% attributeNames) {
    
    stop(
      "No such attribute: ", hsQuoteChr(attributeName), " in ",
      deparse(substitute(x)), ". Available attributes: ",
      stringList(attributeNames), call. = FALSE
    )
  }
  
  attr(x, attributeName)
}

# removeAttributes -------------------------------------------------------------

#' Remove all or selected Attributes of an Object
#' 
#' @param x object
#' @param names names of attributes to be removed. If \code{NULL} (default),
#'   all attributes are removed.
#' @return \emph{x}, but with its attributes removed
#' @export
#' 
removeAttributes <- function(x, names = NULL)
{
  if (is.null(names)) {
    
    attributes(x) <- NULL  
    
  } else {
    
    for (name in names) {
      
      attr(x, name) <- NULL
    }
  }
  
  x
}

# toLookupClass ----------------------------------------------------------------

#' keys and values to lookup structure
#' 
#' provide the mapping between keys and values in a structure of choice
#' 
#' @param keys vector of keys
#' @param values vector of values
#' @param class character string determining the class of the structure returned: 
#'   "data.frame.1": data frame with the \code{keys} as column names and the
#'   \code{values} in the first row; "data.frame.2": data frame with
#'   \code{keys} in the first and \code{values} in the second column; "list":
#'   list with \code{values} as elements and \code{keys} as element names;
#'   "vector": named vector with \code{values} as elements and \code{keys} as
#'   names.
#' 
#' @return object according to the chosen \code{class}. See description of the 
#'   \code{class} argument.
#' 
#' @examples 
#'   keys <- c("A", "B", "C")
#'   values <- c("Apple", "Banana", "Cherry")
#'   
#'   fruits.df1 <- toLookupClass(keys, values)
#'   fruits.df2 <- toLookupClass(keys, values, class = "data.frame.2")
#'   fruits.list <- toLookupClass(keys, values, class = "list")
#'   fruits.vector <- toLookupClass(keys, values, class = "vector")
#'   
#'   # Note how you may use the results differently
#'   
#'   fruits.df1$A
#'   fruits.list$A
#'   fruits.vector["A"]
#'   
#'   fruits.df1[c("A", "C")]
#'   fruits.list[c("A", "C")]
#'   fruits.vector[c("A", "C")]
#'   
#' 
toLookupClass <- structure(
  function # keys and values to lookup structure
### provide the mapping between keys and values in a structure of choice
(
  keys,
  ### vector of keys
  values, 
  ### vector of values
  class = c("data.frame.1", "data.frame.2", "list", "vector")[1]
  ### character string determining the class of the structure returned: 
  ### "data.frame.1": data frame with the \code{keys} as column names and the
  ### \code{values} in the first row; "data.frame.2": data frame with
  ### \code{keys} in the first and \code{values} in the second column; "list":
  ### list with \code{values} as elements and \code{keys} as element names;
  ### "vector": named vector with \code{values} as elements and \code{keys} as
  ### names.
)
{
  classes <- kwb.utils::toNamedList(
    c("data.frame.1", "data.frame.2", "list", "vector")
  )
  
  if (class %in% c(classes$data.frame.1, classes$data.frame.2)) {
    
    kwb.utils::toLookupTable(
      keys, values, as.twoColumnTable = (class == classes$data.frame.2)
    )
    
  } else if (class == classes$list) {
    
    kwb.utils::toLookupList(keys, values)
    
  } else if (class == classes$vector) {
    
    structure(values, names = keys)
    
  } else {
    
    stop("class '", class, "' not supported. Supported classes: ", 
         stringList(classes), call. = FALSE)
  }
  
  ### object according to the chosen \code{class}. See description of the 
  ### \code{class} argument.
}, ex = function() {
  keys <- c("A", "B", "C")
  values <- c("Apple", "Banana", "Cherry")
  
  fruits.df1 <- toLookupClass(keys, values)
  fruits.df2 <- toLookupClass(keys, values, class = "data.frame.2")
  fruits.list <- toLookupClass(keys, values, class = "list")
  fruits.vector <- toLookupClass(keys, values, class = "vector")
  
  # Note how you may use the results differently
  
  fruits.df1$A
  fruits.list$A
  fruits.vector["A"]
  
  fruits.df1[c("A", "C")]
  fruits.list[c("A", "C")]
  fruits.vector[c("A", "C")]
})

# toLookupList -----------------------------------------------------------------

#' keys and values to lookup list
#' 
#' keys and values to lookup list (list with elements representing the values
#'   and the names of the list elements representing the corresponding keys)
#' 
#' @param keys vector of character representing the keys of the dictionary
#' @param values vector of character representing the values of the dictionary
#' @param data Optional. Data frame with two columns of which the first is assumed to
#'   contain the keys and the second is assumed to contain the values of the
#'   dictionary
#' 
#' @return dictionary: list with \emph{values} as elements and \emph{keys} as names
#' 
toLookupList <- function # keys and values to lookup list
### keys and values to lookup list (list with elements representing the values
### and the names of the list elements representing the corresponding keys)
(
  keys, 
  ### vector of character representing the keys of the dictionary
  values, 
  ### vector of character representing the values of the dictionary
  data = NULL
  ### Optional. Data frame with two columns of which the first is assumed to
  ### contain the keys and the second is assumed to contain the values of the
  ### dictionary
) 
{
  if (! is.null(data)) {
    
    if (ncol(data) < 2) {
      stop("data must have at least two columns (keys, values)", call. = FALSE)
    }
    
    lookupMatrix <- t(data)
    keys <- lookupMatrix[1, ]
    values <- lookupMatrix[2, ]
  }
  
  structure(as.list(values), names = keys)
  ### dictionary: list with \emph{values} as elements and \emph{keys} as names
}

# toLookupTable ----------------------------------------------------------------

#' keys and values or list to lookup table
#' 
#' convert vectors of keys and values or a list into a lookup table (data 
#'   frame)
#' 
#' @param keys keys of the lookup table
#' @param values values of the lookup table
#' @param List list of named elements with the names representing the keys
#'   and the values representing the values of the lookup table
#' @param as.twoColumnTable if TRUE (the default is FALSE) the result is a data frame with two columns
#'   \emph{keys} and \emph{values}, respectively.
#' @param stringsAsFactors passed to \code{\link[base]{data.frame}}
#' 
#' @return data frame with one row containing \code{values} in columns named 
#'   \code{keys} or, if \code{as.twoColumnTable = TRUE}, a data frame with the
#'   \code{keys} in column \emph{key} and the \code{values} in column
#'   \emph{value}
#' 
toLookupTable <- function
(
  keys = NULL,
  values = NULL,
  List = NULL,
  as.twoColumnTable = FALSE,
  stringsAsFactors = FALSE
)
{
  # if List is given it must be a list
  if (! is.null(List)) {
    
    if (! is.list(List)) {
      stop("List must be a list!")
    }
    
    keys <- names(List)
    
    # apply the type conversion function corresponding to the type of the first
    # list element to each element to get the vector of values
    values <- unlist(lapply(List, paste0("as.", mode(List[[1]]))))
  }
  
  if (as.twoColumnTable) {
    result <- data.frame(
      key = keys, 
      value = values, 
      stringsAsFactors = stringsAsFactors,
      row.names = NULL
    )
  } else {
    result <- data.frame(
      t(values), 
      stringsAsFactors = stringsAsFactors
    )
    names(result) <- keys
  }
  
  result
}

# tableLookup ------------------------------------------------------------------

#' lookup value for key in table
#' 
#' lookup a value in the second column of a data frame \code{x} where the value
#'   in the first column matches the \code{key}.
#' 
#' @param x data frame with at least two columns. Keys are expected to be in the first
#'   and values are expected to be in the second column, respectively.
#' @param key key for which a value is to be looked up
#' @param default default value that is returned if the key is not a key of the lookup table
#' 
#' @return value looked up in the lookup table of default if \code{key} is not 
#'   contained in the first column of the lookup table \code{x}
#' 
tableLookup <- function # lookup value for key in table
### lookup a value in the second column of a data frame \code{x} where the value
### in the first column matches the \code{key}.
(
  x, 
  ### data frame with at least two columns. Keys are expected to be in the first
  ### and values are expected to be in the second column, respectively.
  key, 
  ### key for which a value is to be looked up
  default = NULL
  ### default value that is returned if the key is not a key of the lookup table
)
{
  stopifnot(is.data.frame(x))
  stopifnot(ncol(table) >= 2)
  
  keys <- x[, 1]
  
  selected <- (keys == key)
  
  if (! any(selected)) {
    
    warning(
      "No key ", hsQuoteChr(key), " found in the lookup table.\n",
      "Available keys: ", stringList(as.character(keys)), ".\n",
      "Returning the default value: ", hsQuoteChr(default)
    )
    
    value <- default
    
  } else {
    
    value <- x[selected, 2]
    
    if (length(value) > 1) {
      
      value <- value[1]
      
      warning(
        "The key ", hsQuoteChr(key), " occurs more than once in the lookup ",
        "table.\nReturning the first value found: ", hsQuoteChr(value)
      )
    }
  }
  
  value
  ### value looked up in the lookup table of default if \code{key} is not 
  ### contained in the first column of the lookup table \code{x}
}

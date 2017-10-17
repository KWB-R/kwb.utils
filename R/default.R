# defaultLevels ----------------------------------------------------------------

#' Default Factor Levels
#' 
#' If x is numeric and all values are whole numbers use the sequence between the
#' smallest and the highest value
#' 
#' @param x numeric or character vector
#' @param step step to be used in sequence generation
#'   
#' @return if \code{x} is numeric the sequence between the lowest and highest
#'   value in \code{x} with the given \code{step} is returned. If \code{x} is a 
#'   vector of character strings, the sorted unique values are returned.
#'   
#' @examples 
#' defaultLevels(c(1, 3, 4, 5, 4))
#' defaultLevels(c(1920, 1950, 1970), step = 10)
#' 
defaultLevels <- function(x, step = 1)
{
  if (is.numeric(x)) {
    
    x.non.na <- x[! is.na(x)]
    
    quotient <- x.non.na / step
    
    if (all(quotient - round(quotient) < 1e-16)) {
      
      range.x <- range(x.non.na)
      
      x <- seq(from = range.x[1], to = range.x[2], by = step)
    }
  }
  
  sort(unique(x))
}

# defaultIfNULL ----------------------------------------------------------------

#' Default Value if Object is NULL
#' 
#' Return the given object or a default value if the object is NULL
#' 
#' @param x R object to be checked for NULL
#' @param default default value that is returned if \code{x} is NULL
#' @param count if \code{TRUE} (the default is \code{FALSE}) the number of
#'   replaced values is returned in the attributes \code{count}
#'   
#' @return \code{x} if \code{x} is not NULL and \code{default} otherwise
#'   
#' @seealso \code{\link{defaultIfNA}, \link{defaultIfZero}}
#'   
#' @examples 
#' defaultIfNULL(NULL, "default") # returns the default value
#' defaultIfNULL("actual", "default") # returns the "actual" value
#'   
defaultIfNULL <- function(x, default, count = FALSE)
{
  .defaultIf(is.null, x, default, count = count)
}

# defaultIfNA ------------------------------------------------------------------

#' Default Value if Object is NA
#' 
#' Return the given object or a default value if the object is NA
#' 
#' @param x vector possibly containing NA values
#' @param default default value that is returned if \code{x} is NA
#' @param count if \code{TRUE} (the default is \code{FALSE}) the number of
#'   replaced values is returned in the attributes \code{count}
#'   
#' @return \code{x} if \code{x} is not NA and \code{default} otherwise
#'   
#' @seealso \code{\link{defaultIfNULL}, \link{defaultIfZero}}
#'   
#' @examples 
#' defaultIfNA(NA, "default") # returns the default value
#' defaultIfNA("actual", "default") # returns the "actual" value
#'
defaultIfNA <- function(x, default, count = FALSE)
{
  .defaultIf(is.na, x, default, count = count)
}

# defaultIfZero ----------------------------------------------------------------

#' Default Value if Object is 0 (zero)
#' 
#' Return the given object or a default value if the object is 0 (zero)
#' 
#' @param x vector possibly containing zeroes
#' @param default value to be used instead of zero
#' @param count if \code{TRUE} (the default is \code{FALSE}) the number of
#'   replaced values is returned in the attributes \code{count}
#'   
#' @return \code{x} if \code{x} is not 0 and \code{default} otherwise. The
#'   returned object has an attribute \code{count} containing the number of
#'   values that have been set to the default value.
#'   
#' @seealso \code{\link{defaultIfNA}, \link{defaultIfNULL}}
#'   
#' @examples 
#' defaultIfZero(c(1, 2, 0, 4, 5, 0, 6), NA) # returns the default value (NA) 
#' (out <- defaultIfZero(1:6, NA, count = TRUE)) # returns the "actual" values
#' 
#' # The number of values that were zero is returned in the attribute "count" 
#' attr(out, "count")
#'   
defaultIfZero <- function(x, default, count = FALSE)
{
  .defaultIf(function(x) x == 0, x, default, count = count)
}

# .defaultIf -------------------------------------------------------------------

#' Return a Default Value for Values Meeting a Condition
#' 
#' @param count if \code{TRUE} (the default is \code{FALSE}) the number of
#'   replaced values is returned in the attributes \code{count}
#' 
.defaultIf <- function(FUN.test, x, default, count = FALSE)
{
  selected <- if (length(x) > 1) {
    sapply(x, FUN.test)
  } else {
    FUN.test(x)
  }
  
  if (any(selected)) {
    
    if (is.null(x)) {
      x <- default
    }
    else {
      x[selected] <- default
    }
  }
  
  if (count) {
    x <- structure(x, count = sum(selected, na.rm = TRUE))
  }
  
  x
}

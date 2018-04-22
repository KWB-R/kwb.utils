# Functions returning logical

# almostEqual ------------------------------------------------------------------

#' Compare Numerical Vectors with Tolerance
#' 
#' Compare numerical vectors accepting a tolerance
#' 
#' @param x vector of numerical
#' @param y vector of numerical
#' @param tolerance tolerance, i.e. accepted difference between values in
#'   \code{x} and \code{y}. Default: 1e-12
#' 
#' @return vector of logical
#' 
almostEqual <- function(x, y, tolerance = 1e-12)
{
  stopifnot(is.numeric(x))
  
  stopifnot(is.numeric(y))
  
  stopifnot(length(x) == length(y))
  
  abs(x - y) < tolerance
}

# matchesCriteria --------------------------------------------------------------

#' Do Data Frame Row Match Given Criteria?
#' 
#' are data frame rows matching given criteria?
#' 
#' @param Data data frame
#' @param criteria vector of character containing conditions, in which the
#'   column names of \code{Data}, e.g. \code{A} can appear unquoted, e.g.
#'   \code{"A == 'x'"}
#' @param na.to.false if \code{TRUE} (the default is \code{FALSE}) NA in the
#'   resulting vector will be replaced with FALSE
#' @param add.details if \code{TRUE} (the default is \code{FALSE}) a matrix 
#'   containing the evaluation of each criterion is returned in attribute 
#'   \code{details}
#' @param dbg if TRUE (default) for each criterion in \code{criteria} it is
#'   shown for how many rows in \code{Data} the criterion is TRUE and for how
#'   many rows it is FALSE
#' 
#' @return vector of logical containing TRUE at positions representing rows in
#'   \code{Data} fulfilling the \code{conditions} and FALSE elsewhere
#' 
#' @examples 
#' # Define an example data frame
#' Data <- data.frame(A = c("x", "y", "z", NA),
#'                    B = c( NA,   2,   3, 4))
#' 
#' # Define one or more criteria
#' criteria <- c("A %in% c('y', 'z')", "B %in% 1:3")
#' 
#' # For which rows the criteria are met (vector of logical)?
#' matchesCriteria(Data, criteria, dbg = FALSE)
#' 
#' # You may use the function in the context of indexing:
#' Data[matchesCriteria(Data, criteria), ]
#' 
#' # Filtering for non-NA values
#' D1 <- Data[matchesCriteria(Data, "! is.na(A) & ! is.na(B)"), ]
#' 
#' # the same result is returned by:
#' D2 <- Data[matchesCriteria(Data, c("! is.na(A)", "! is.na(B)")), ]
#' 
#' identical(D1, D2)
#' 
matchesCriteria <- function(
  Data, criteria = NULL, na.to.false = FALSE, add.details = FALSE, dbg = TRUE
)
{
  stopifnot(is.data.frame(Data))
  
  selected <- rep(TRUE, nrow(Data))
  
  if (is.null(criteria) || length(criteria) == 0 || all(is.na(criteria))) {
    
    catIf(dbg, "No search criteria given. Returning TRUE for all rows.\n")
    
    return (selected)
  }
  
  stopifnot(is.character(criteria))
  
  N <- nrow(Data)
  
  details <- list()
  
  for (criterion in criteria) {
    
    expr <- parse(text = criterion, keep.source = FALSE)
    
    catIf(dbg, "Evaluating", as.character(expr), "...\n")

    result <- eval(expr, Data)
    
    if (! is.logical(result) || length(result) != N) {
      
      stop(sprintf(
        paste("The expression '%s' does not return a logical vector",
              "of length %d but: %s"), 
        as.character(expr), N, as.character(result)
      ))
    }
    
    # Save the result for the current criterion in the details list
    details[[length(details) + 1]] <- result
    
    selected <- selected & result
    
    sum.true <- sum(result, na.rm = TRUE)
    sum.false <- sum(! result, na.rm = TRUE)
    sum.na <- sum(is.na(result))
    
    catIf(dbg, sprintf(
      "  is TRUE for %7d rows (%5.1f %%),\n", 
      sum.true, percentage(sum.true, basis = N)
    ))
    
    catIf(dbg, sprintf(
      "    FALSE for %7d rows (%5.1f %%) and\n", 
      sum.false, percentage(sum.false, basis = N)
    ))
    
    catIf(dbg, sprintf(
      "       NA for %7d rows (%5.1f %%).\n", 
      sum.na, percentage(sum.na, basis = N)
    ))
    
    catIf(dbg, sprintf(
      "  Selected rows now: %d\n", sum(selected, na.rm = TRUE)
    ))
  }
  
  sum.na <- sum(is.na(selected))
  
  if (sum.na > 0) {
    
    if (na.to.false) {
      
      selected[is.na(selected)] <- FALSE
      
      message(sum.na, " NAs have been set to FALSE in the vector returned ",
              "by matchesCriteria().")
      
    } else {
      
      warning(
        "There are ", sum.na, " NAs in the returned vector. This will lead ", 
        "to NA-rows when used in the context of selecting rows!"
      )
    }
  }
  
  # If required, append the list of vectors for each criterion in attribute
  # "details"
  if (add.details) {
    
    selected <- structure(
      selected, 
      details = .detailsListToMatrix(details, criteria)
    )
  }
  
  selected
}

# .detailsListToMatrix ---------------------------------------------------------

.detailsListToMatrix <- function(x, criteria)
{
  out <- as.matrix(do.call(cbind, x))
  
  ids <- paste0("C", seq_along(x))
  
  colnames(out) <- ids
  
  metadata <- data.frame(id = ids, condition = criteria)
  
  structure(out, criteria = metadata)
}

# allAreEqual ------------------------------------------------------------------

#' are all elements in x the same?
#' 
#' @param x vector of elements to be compared
#' @param method Select one of two methods. 1: check if the length of unique
#'   elements is equal to one, 2: check if all elements are equal to the first
#'   element.
#'   
#' @return \code{TRUE} if all elements in \code{x} are equal to each other,
#'   otherwise \code{FALSE}
#' 
allAreEqual <- function(x, method = 1)
{
  if (method == 1) {
    
    length(unique(x)) == 1
    
  } else {
    
    all(x == x[1])
  }
}

# allAreIdentical --------------------------------------------------------------

#' Are all list elements identical to each other?
#' 
#' @param x a list
#' 
#' @return \code{TRUE} if all elements in \code{x} are identical, otherwise 
#'   \code{FALSE}
#' 
allAreIdentical <- function(x)
{
  name.x <- hsQuoteChr(deparse(substitute(x)))
  
  if (! is.list(x)) {
    
    stop(
      name.x, " is assumed to be a list! Use allAreEqual() to compare all ", 
      "elements of a vector.", call. = FALSE
    )
  }
  
  L <- length(x)
  
  if (L == 0) {
    
    stop("The length of ", name.x, " must be at least one!", call. = FALSE)
    
  } else if (L == 1) {
    
    message("There is only one element in ", name.x, 
            ". So all elements are equal.")
    TRUE
    
  } else {
    
    all(sapply(seq(2, L), function(i) identical(x[[1]], x[[i]])))
  }
}

# isNullOrEmpty ----------------------------------------------------------------

#' isNullOrEmpty
#' 
#' @param x object to be tested for NULL or being empty (vector or list of
#'   length 0 or data frame with no rows)
#'   
#' @return TRUE if x is NULL or x is a vector of length 0 or x is a data frame
#'   with no rows.
#' 
isNullOrEmpty <- function(x)
{
  is.null(x) || 
    (is.vector(x) && (length(x) == 0)) || 
    (is.list(x) && (length(x) == 0)) ||
    (is.data.frame(x) && (nrow(x) == 0))
}

# isNaOrEmpty ------------------------------------------------------------------

#' NA or the empty string ""?
#' 
#' is an object NA or equal to the empty string "" (after trimming)?
#' 
#' @param x object to be tested for NA or being empty (equal to "", after trimming)
#' 
#' @return (vector of) logical, being TRUE for each element in \emph{x} that is
#'   NA or the empty string "" (after trimming)
#' 
isNaOrEmpty <- function(x)
{
  is.na(x) | hsTrim(x) == ""
}

# isNaInAllColumns -------------------------------------------------------------

#' isNaInAllColumns
#' 
#' @param dataFrame data frame or matrix
#' 
#' @return logical vector with as many elements as there are rows in 
#'   \emph{dataFrame} (TRUE for rows in which all elements are NA, FALSE for
#'   rows in which there is at least one non-NA element).
#' 
isNaInAllColumns <- function(dataFrame)
{
  stopifnot(is.data.frame(dataFrame) | is.matrix(dataFrame))
  
  # use as.logical to remove names
  as.logical(rowSums(as.matrix(is.na(dataFrame))) == ncol(dataFrame))
}

# isNaInAllRows ----------------------------------------------------------------

#' isNaInAllRows
#' 
#' @param dataFrame data frame or matrix
#' 
#' @return logical vector with as many elements as there are columns in 
#'   \emph{dataFrame} (TRUE for columns in which all elements are NA, FALSE for
#'   columns in which there is at least one non-NA element).
#' 
isNaInAllRows <- function(dataFrame)
{
  stopifnot(is.data.frame(dataFrame) | is.matrix(dataFrame))

  # use as.logical to remove names
  as.logical(colSums(as.matrix(is.na(dataFrame))) == nrow(dataFrame))
}

# isOddNumber ------------------------------------------------------------------

#' Check for Odd Numbers
#' 
#' @param x vector of numeric
#' 
isOddNumber <- function(x) 
{
  x %% 2 != 0
}

# isEvenNumber -----------------------------------------------------------------

#' Check for Even Numbers
#' 
#' @param x vector of numeric
#' 
isEvenNumber <- function(x) 
{
  x %% 2 == 0
}

# inRange ----------------------------------------------------------------------

#' check for values within minimum and maximum value
#' 
#' @param values vector of values
#' @param min.value minimum value (inclusive)
#' @param max.value maximum value (inclusive)
#' 
#' @return vector of boolean
#' 
inRange <- function(values, min.value, max.value) 
{
  (values >= min.value) & (values <= max.value)
}

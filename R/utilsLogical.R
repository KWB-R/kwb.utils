#
# Functions returning logical --------------------------------------------------
#

# allAreEqual ------------------------------------------------------------------
allAreEqual <- function # allAreEqual
### allAreEqual
(elements)
{
  length(unique(elements)) == 1
}

# isNullOrEmpty ----------------------------------------------------------------
isNullOrEmpty <- function # isNullOrEmpty
### isNullOrEmpty
(
  x
  ### object to be tested for NULL or being empty (vector or list of length 0 or 
  ### data frame with no rows)
)
{
  is.null(x) || 
    (is.vector(x) && (length(x) == 0)) || 
    (is.list(x) && (length(x) == 0)) ||
    (is.data.frame(x) && (nrow(x) == 0))
  
  ### TRUE if x is NULL or x is a vector of length 0 or x is a data frame with
  ### no rows.
}

# isNaOrEmpty ------------------------------------------------------------------
isNaOrEmpty <- function # NA or the empty string ""?
### is an object NA or equal to the empty string "" (after trimming)?
(
  x
  ### object to be tested for NA or being empty (equal to "", after trimming)
)
{
  is.na(x) | hsTrim(x) == ""
  ### (vector of) logical, being TRUE for each element in \emph{x} that is
  ### NA or the empty string "" (after trimming)
}

# isNaInAllColumns -------------------------------------------------------------
isNaInAllColumns <- function # isNaInAllColumns
### isNaInAllColumns
(
  dataFrame
  ### data frame or matrix
)
{
  stopifnot(is.data.frame(dataFrame) | is.matrix(dataFrame))
  
  # use as.logical to remove names
  as.logical(rowSums(as.matrix(is.na(dataFrame))) == ncol(dataFrame))
  
  ### logical vector with as many elements as there are rows in 
  ### \emph{dataFrame} (TRUE for rows in which all elements are NA, FALSE for
  ### rows in which there is at least one non-NA element).
}

# isNaInAllRows ----------------------------------------------------------------
isNaInAllRows <- function # isNaInAllRows
### isNaInAllRows
(
  dataFrame
  ### data frame or matrix
)
{
  stopifnot(is.data.frame(dataFrame) | is.matrix(dataFrame))

  # use as.logical to remove names
  as.logical(colSums(as.matrix(is.na(dataFrame))) == nrow(dataFrame))
  
  ### logical vector with as many elements as there are columns in 
  ### \emph{dataFrame} (TRUE for columns in which all elements are NA, FALSE for
  ### columns in which there is at least one non-NA element).
}

# allTheSame -------------------------------------------------------------------
allTheSame <- function
### are all elements in x the same?
(
  x
  ### vector of elements to be compared
) 
{
  all(x == x[1])
}

# isOddNumber ------------------------------------------------------------------
isOddNumber <- function # check for odd numbers
### check for odd numbers
(
  x
) 
{
  x %% 2 != 0
}

# isEvenNumber -------------------------------------------------------------------------
isEvenNumber <- function # check for even numbers
### check for even numbers
(
  x
) 
{
  x %% 2 == 0
}

# inRange ----------------------------------------------------------------------
inRange <- function # inRange
### check for values within minimum and maximum value
(
  values, 
  ### vector of values
  min.value, 
  ### minimum value (inclusive)
  max.value
  ### maximum value (inclusive)
) 
{
  (values >= min.value) & (values <= max.value)
  ### vector of boolean
}

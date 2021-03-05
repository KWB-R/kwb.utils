# multiColumnLookup ------------------------------------------------------------

#' Lookup by Matching Values in Multiple Columns
#' 
#' @param data data frame for which to lookup values in the \code{lookup} table
#' @param lookup lookup table defining the key values to be matched against
#'   the values in the corresponding columns in \code{data} and the 
#'   corresponding lookup values that are to be returned
#' @param value name of column in \code{lookup} containing the value to be 
#'   looked up. Default: name of the last column in \code{lookup}
#' @return vector with as many elements as there are rows in \code{data}.
#' @export
#' @examples 
#' (persons <- rbind(
#'   noFactorDataFrame(name = "Peter", city = "Berlin"),
#'   noFactorDataFrame(name = "Paul", city = "Paris"),
#'   noFactorDataFrame(name = "Mary", city = "Berlin"),
#'   noFactorDataFrame(name = "Paul", city = "Berlin"),
#'   noFactorDataFrame(name = "Peter", city = "Paris")
#' ))
#' 
#' # Who is cool, which city is cool and which combination is coolest?
#' (is_cool <- kwb.utils::safeRowBindAll(list(
#'   noFactorDataFrame(name = "Paul", city = "Berlin", value = "astro-cool"),
#'   noFactorDataFrame(city = "Berlin", value = "cool"),
#'   noFactorDataFrame(name = "Paul", value = "mega-cool"),
#'   noFactorDataFrame(city = "Paris", value = "ca va")
#' )))
#' 
#' # Lookup the coolness based on name and city
#' coolness <- multiColumnLookup(persons, is_cool, value = "value")
#' 
#' # Add the coolness column
#' cbind(persons, coolness)
#' 
multiColumnLookup <- function(data, lookup, value = NULL)
{
  stopifnot(is.data.frame(data))
  stopifnot(is.data.frame(lookup))
  
  value <- defaultIfNULL(value, names(lookup)[ncol(lookup)])
  
  checkForMissingColumns(lookup, value)
  
  # Extract the key columns into data frame "keys"  
  keys <- removeColumns(lookup, value)
  
  # Create list of rows from "keys" data frame. Each row represents a 
  # combination of values in different columns that are to be looked up
  raw_criteria <- split(keys, seq_len(nrow(lookup)))
  
  # Remove empty fields from the raw criteria
  criteria <- lapply(raw_criteria, function(x) {
    x[, ! isNaOrEmpty(x), drop = FALSE]
  })
  
  # Apply each criterion to the input data frame "data". Return a matrix of 
  # logical with one row per row in "data" and one column per criterion. 
  match_matrix <- do.call(cbind, lapply(criteria, function(criterion) {
    
    Reduce(`&`, lapply(names(criterion), function(column) {
      
      selectColumns(data, column) == criterion[1L, column]
    }))
  }))
  
  # For each row in match_matrix, find the index of the first "TRUE"
  indices <- sapply(asRowList(match_matrix), function(x) unname(which(x)[1L]))
  
  is_na <- is.na(indices)
  
  if (any(is_na)) {
    
    printIf(TRUE, utils::head(data[is_na, ]))
    printIf(TRUE, lookup)
    
    warning(
      "For ", sum(is_na), " observations in ", deparse(substitute(data)), 
      " (first rows see above) no lookup value could be found", call. = FALSE
    )
  }
  
  # Lookup the name of the stratum corresponding to the first matchin criterion
  selectColumns(lookup, value)[indices]
}

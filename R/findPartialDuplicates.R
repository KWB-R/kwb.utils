# findPartialDuplicates --------------------------------------------------------

#' Find Paritally Duplicated Rows in a Data Frame
#'
#' Find Rows in a data frame that are identical in the key columns but not
#' identical in all columns
#'
#' @param data data frame
#' @param key_columns names of columns in \code{data} in which to look for
#'   duplicated (combined) values
#' @param skip_columns names of columns to be skipped when looking for
#'   duplicated rows
#' @return \code{NULL} if there are no rows in \code{data} that have identical
#'   values in the \code{key_columns} or if all groups of rows that have
#'   identical values in the \code{key_columns} are also identical in all the
#'   other columns (except for those named in \code{skip_columns}). Otherwise
#'   a list is returned with the one element per duplicate in the key columns.
#'   The list elements are subsets of \code{data} representing the rows of
#'   \code{data} that are identical in the key columns but different in at least
#'   one of the other columns.
#' @export
#' @examples
#' findPartialDuplicates(key_columns = "id", data = rbind(
#'   data.frame(id = 1, value = 1),
#'   data.frame(id = 2, value = 2),
#'   data.frame(id = 2, value = 3),
#'   data.frame(id = 3, value = 3),
#'   data.frame(id = 3, value = 3),
#'   data.frame(id = 3, value = 3.1)
#' ))
#'
findPartialDuplicates <- function(data, key_columns, skip_columns = NULL)
{
  keys <- selectColumns(data, key_columns, drop = FALSE)

  # Columns to be considered in duplicate check
  columns <- setdiff(names(data), skip_columns)
  
  # Are the rows identical in all columns (except those in skip_columns)?
  is_row_duplicate <- duplicated(data[, columns])

  # Are the keys (combination of key values) duplicated?
  is_key_duplicate <- duplicated(keys)
  
  # Are the rows duplicated in the key columns but not fully duplicated rows?
  is_partial_duplicate <- is_key_duplicate & ! is_row_duplicate

  # For any partial duplicate, extract the differing values
  if (any(is_partial_duplicate)) {
    
    lapply(which(is_partial_duplicate), function(index) {
      
      # Get the value(s) of the (composed) key
      values <- keys[index, , drop = FALSE]
      
      # Which rows are matching in all key columns?
      matches <- sapply(names(values), function(x) values[[x]] == keys[[x]])
      match_indices <- which(rowSums(matches) == length(values))
      
      # Select the corresponding rows
      result <- data[match_indices, , drop = FALSE]
      
      # Select only columns in which values are actually differing
      columns <- c(key_columns, names(result)[! sapply(result, allAreEqual)])
      result[, columns, drop = FALSE]
    })
  }
}

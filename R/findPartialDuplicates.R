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
  row_sets <- unname(split(
    x = seq_len(nrow(data)), 
    f = selectColumns(data, key_columns, drop = FALSE), 
    drop = TRUE
  ))
  
  row_sets <- row_sets[lengths(row_sets) > 1L]
  
  if (length(row_sets) == 0L) {
    return(NULL)
  }
  
  row_sets <- row_sets[order(sapply(row_sets, "[", 1L))]
  
  columns_to_consider <- setdiff(names(data), skip_columns)
  
  result <- lapply(row_sets, function(rows) {
    y <- data[rows, columns_to_consider]
    differing_columns <- names(which(!sapply(y, kwb.utils::allAreEqual)))
    y[, c(key_columns, differing_columns), drop = FALSE]
  })
  
  stats::setNames(result, NULL)
}

#' Remove Leading/Trailing Empty Rows/Columns from Matrix
#'
#' @param m a matrix
#' @return \code{m} with leading trailing empty rows (full of \code{NA}) removed
#' @export
clipMatrix <- function(m)
{
  stopIfNotMatrix(m)
  
  is_na <- is.na(m)
  
  n_na_in_row <- rowSums(is_na)
  n_na_in_col <- colSums(is_na)
  
  row_ranges <- findChanges(n_na_in_row == ncol(m))
  col_ranges <- findChanges(n_na_in_col == nrow(m))
  
  index_seq <- function(x, topleft) {
    i <- if (topleft) 1L else nrow(x)
    if (x$value[i]) x$starts_at[i]:x$ends_at[i] # else NULL
  }
  
  i_remove <- c(index_seq(row_ranges, TRUE), index_seq(row_ranges, FALSE))
  j_remove <- c(index_seq(col_ranges, TRUE), index_seq(col_ranges, FALSE))
  
  if (length(i_remove)) m <- m[-i_remove, , drop = FALSE]
  if (length(j_remove)) m <- m[, -j_remove, drop = FALSE]
  
  m
}

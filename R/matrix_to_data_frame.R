# matrixToDataFrame ------------------------------------------------------------

#' Convert a Matrix to a Data Frame (in "Long" Format)
#'
#' @param x matrix
#' @param name_row name to be given to the data frame column containing the
#'   row "coordinates". Default: \code{names(dimnames(x))[1]} unless
#'   \code{NULL}, \code{"row"} otherwise.
#' @param name_column name to be given to the data frame column containing the
#'   column "coordinates". Default: \code{names(dimnames(x))[2]} unless
#'   \code{NULL}, \code{"column"} otherwise.
#' @param name_value name to be given to the data frame column containing the
#'   matrix values. Default: \code{value}
#' @param row_first if \code{TRUE} (the default), the "row column" will come
#'   first, else the "column column".
#' @return data frame with three columns: 1. row "coordinate", 2. column
#'   "coordinate", 3. value
#' @export
#' @examples
#' m1 <- matrix(1:12, nrow = 3, dimnames = list(NULL, letters[1:4]))
#' m2 <- matrix(1:12, nrow = 3, dimnames = list(index = NULL, letters[1:4]))
#' m3 <- matrix(1:12, nrow = 3, dimnames = list(NULL, letter = letters[1:4]))
#'
#' matrixToDataFrame(x = m1)
#' matrixToDataFrame(x = m1, row_first = FALSE)
#' matrixToDataFrame(x = m2)
#' matrixToDataFrame(x = m3)
#' matrixToDataFrame(x = m3, "myrow", "mycol", "myval")
#'
matrixToDataFrame <- function(
  x, name_row = NULL, name_column = NULL, name_value = "value", row_first = TRUE
)
{
  #kwb.utils::assignArgumentDefaults(matrixToDataFrame)
  stopIfNotMatrix(x)

  # Shortcut
  default <- function(x, d) if (is.null(x) || all(x == "")) d else x

  dn <- dimnames(x)
  dnn <- names(dn)

  # Use dimension name as column name in result data frame or default
  name_row <- default(name_row, default(dnn[1L], "row"))
  name_col <- default(name_column, default(dnn[2], "column"))

  # Provide defaults for column names and row names
  row_names <- default(dn[[1L]], seq_len(nrow(x)))
  col_names <- default(dn[[2L]], seq_len(ncol(x)))

  # Prepare list of arguments to data.frame()
  args <- list()

  if (row_first) {

    args[[name_row]] <- rep(row_names, each = ncol(x))
    args[[name_col]] <- col_names

  } else {

    args[[name_col]] <- rep(col_names, each = nrow(x))
    args[[name_row]] <- row_names
  }

  # Remove attributes
  args[[name_value]] <- `attributes<-`(if (row_first) t(x) else x, NULL)

  do.call(data.frame, c(args, list(stringsAsFactors = FALSE)))
}

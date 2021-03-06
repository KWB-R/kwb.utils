% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{assertRowsAndColumns}
\alias{assertRowsAndColumns}
\title{Assert Row and Column Names of a Matrix}
\usage{
assertRowsAndColumns(x, row_names = NULL, col_names = NULL, fill_value = 0)
}
\arguments{
\item{x}{A matrix}

\item{row_names}{character vector of row names}

\item{col_names}{character vector of column names}

\item{fill_value}{value to fill a row or column with if a row or column does
not exist in \code{x}}
}
\description{
Make sure that a matrix contains rows and columns of the given names in the
given order.
}
\examples{
m <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(
  rows = paste0("row", 1:3), cols = paste0("col", 1:4)
))

# Add two rows, reverse order of rows, add one column, remove one column
assertRowsAndColumns(
  m,
  row_names = paste0("row", 4:0),
  col_names = paste0("col", 0:2)
)

}

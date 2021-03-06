% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{asColumnList}
\alias{asColumnList}
\title{Matrix to List of Matrix Columns}
\usage{
asColumnList(x)
}
\arguments{
\item{x}{matrix}
}
\value{
list with as many elements as there are columns in \code{x} and each
  element representing one column
}
\description{
Matrix to List of Matrix Columns
}
\examples{
x <- matrix(1:12, nrow = 3)

column_list <- asColumnList(x)

for (i in 1:ncol(x)) print(identical(column_list[[i]], x[, i]))

}

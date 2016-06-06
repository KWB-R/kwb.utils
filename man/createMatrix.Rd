\name{createMatrix}
\alias{createMatrix}
\title{matrix with row and column names}
\description{Create a matrix by giving row and column names and with all elements being
set to a default value}
\usage{createMatrix(rowNames, colNames = rowNames, value = 0)}
\arguments{
  \item{rowNames}{character vector of row names to be given to the matrix}
  \item{colNames}{character vector of column names to be given to the matrix}
  \item{value}{value to be given to each matrix element}
}

\value{matrix with \code{rowNames} as row names and \code{colNames} as column
names, filled with \emph{value} at each position}

\author{Hauke Sonnenberg}




\examples{
## Initialise a matrix with rows A to E and columns x to z of value -1
createMatrix(c("A", "B", "C", "D", "E"), c("x", "y", "z"), -1)

## By default the column names are assumed to be equal to the row names
createMatrix(c("A", "B", "C"))

## Initialise a square matrix with NA
createMatrix(c("A", "B", "C"), value = NA)
}

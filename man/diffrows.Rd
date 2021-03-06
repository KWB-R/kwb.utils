% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{diffrows}
\alias{diffrows}
\title{Differences between Matrix Rows}
\usage{
diffrows(x)
}
\arguments{
\item{x}{matrix}
}
\value{
matrix with one row less than in input matrix \code{x} and each row 
  \code{i} representing the difference \code{x[i+1, ]-x[i, ]} between rows
  \code{i+1} and \code{i} in \code{x}
}
\description{
Differences between Matrix Rows
}
\examples{
x <- matrix(1:12, nrow = 3)

d <- diffrows(x)

x[2, ] - x[1, ] == d[1, ]
x[3, ] - x[2, ] == d[2, ]

}

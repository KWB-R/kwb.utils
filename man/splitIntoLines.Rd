% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitIntoLines.R
\name{splitIntoLines}
\alias{splitIntoLines}
\title{Split Text at End of Line Characters Into Lines}
\usage{
splitIntoLines(x)
}
\arguments{
\item{x}{vector of character of length 1.}
}
\value{
vector of character as long as there are lines in \code{x}
}
\description{
Split Text at End of Line Characters Into Lines
}
\examples{
splitIntoLines("a\nb\nc")
splitIntoLines("a\r\nb\r\nc")
}

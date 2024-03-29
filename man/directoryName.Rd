% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directoryName.R
\name{directoryName}
\alias{directoryName}
\title{Safe Version of Base Function dirname()}
\usage{
directoryName(x)
}
\arguments{
\item{x}{a file path of which to get the path to the directory only}
}
\value{
path to directory of file path given in \code{x}
}
\description{
The base function \code{\link{dirname}} may fail if the path passed is too
long. This version checks if the call of \code{dirname()} failed and gives 
a clear error message.
}
\examples{
\dontrun{
directoryName(repeated("verylongpath/", 50))
}
}

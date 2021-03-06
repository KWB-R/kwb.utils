% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeArgs.R
\name{mergeLists}
\alias{mergeLists}
\title{Merge Lists Overriding Elements of the Same Name}
\usage{
mergeLists(..., warn.on.NULL = TRUE)
}
\arguments{
\item{\dots}{lists}

\item{warn.on.NULL}{if TRUE (default) a warning is given if any of the
arguments given to this function is NULL}
}
\value{
list containing the elements given in \code{...}
}
\description{
Merge Lists Overriding Elements of the Same Name
}
\examples{
# merge two lists with different elements
mergeLists(list(a = 1), list(b = 2))

# merge two lists with one element of the same name: override element "b"
mergeLists(list(a = 1, b = 2), list(b = 3, c = 4))

}
\seealso{
\code{\link{arglist}}
}

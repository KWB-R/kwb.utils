\name{merge.lists}
\alias{merge.lists}
\title{merge lists overriding elements of the same name}
\description{merge lists overriding elements of the same name}
\usage{\method{merge}{lists}(..., warn.on.NULL = TRUE)}
\arguments{
  \item{\dots}{lists}
  \item{warn.on.NULL}{if TRUE (default) a warning is given if any of the arguments given to
this function is NULL}
}

\value{list containing the elements given in \code{...} }

\author{Hauke Sonnenberg}



\seealso{\code{\link{arglist}}}
\examples{
# merge two lists with different elements
merge.lists(list(a = 1), list(b = 2))

# merge two lists with one element of the same name: override element "b"
merge.lists(list(a = 1, b = 2), list(b = 3, c = 4))
}

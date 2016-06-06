\name{is.unnamed}
\alias{is.unnamed}
\title{are list elements unnamed?}
\description{returns a vector of logical as long as \emph{x} holding TRUE at indices
where the list element at the same indices are named and FALSE at positions
where the list element at the same indices are not named.}
\usage{is.unnamed(x)}
\arguments{
  \item{x}{list}
}

\value{vector of logical}

\author{Hauke Sonnenberg}




\examples{
is.unnamed(list(1, b = 2)) # TRUE FALSE
is.unnamed(list(a = 1, 2)) # FALSE TRUE
is.unnamed(list()) # logical(0)
is.unnamed(list(a = 1, 2, c = 3)) # FALSE  TRUE FALSE
}

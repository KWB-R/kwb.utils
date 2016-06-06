\name{startsToEnds}
\alias{startsToEnds}
\title{helper function: start indices to end indices}
\description{helper function to convert start indices to end indices}
\usage{startsToEnds(starts, lastStop, stopOffset = 1)}
\arguments{
  \item{starts}{vector of integer}
  \item{lastStop}{number to be returned as last element of the result vector}
  \item{stopOffset}{number to be subtracted from (all but the first elements in) \emph{starts}
in order to find the ends}
}

\value{vector of integer}

\author{Hauke Sonnenberg}




\examples{
starts <- c(1, 10, 20, 35)

ok <- identical(startsToEnds(starts, lastStop = 50), 
                c(9, 19, 34, 50)) 

ok <- ok && identical(startsToEnds(starts, lastStop = 50, stopOffset = 2), 
                      c(8, 18, 33, 50))
ok
}

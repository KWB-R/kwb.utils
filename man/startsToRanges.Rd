\name{startsToRanges}
\alias{startsToRanges}
\title{row numbers of start rows to from/to row ranges}
\description{a vector of row numbers is transformed to a data frame describing 
row ranges by numbers of  first and last rows}
\usage{startsToRanges(starts, lastStop, startOffset = 1, stopOffset = 1)}
\arguments{
  \item{starts}{
}
  \item{lastStop}{
}
  \item{startOffset}{
}
  \item{stopOffset}{
}
}



\author{Hauke Sonnenberg}




\examples{
starts <- c(1, 10, 20, 35)

ok <- identical(startsToRanges(starts, lastStop = 50), 
                data.frame(
                  from = c(2, 11, 21, 36),
                  to = c(9, 19, 34, 50)
                ))

ok <- ok && identical(
  startsToRanges(starts, lastStop = 55, startOffset = 2, stopOffset = 2), 
  data.frame(
    from = c(3, 12, 22, 37),
    to = c(8, 18, 33, 55)
  ))

ok
}

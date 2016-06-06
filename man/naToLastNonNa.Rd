\name{naToLastNonNa}
\alias{naToLastNonNa}
\title{replace NA values with "last" non-NA value}
\description{replace NA values in a vector with the "last" non-NA values (at the nearest
smaller indices in each case) in the vector}
\usage{naToLastNonNa(x, method = 2)}
\arguments{
  \item{x}{
}
  \item{method}{
}
}



\author{Hauke Sonnenberg}




\examples{
naToLastNonNa(c(1, 2, NA, NA, 3, NA, NA, 4, NA, NA, 5)) 
## Result: [1] 1 2 2 2 3 3 3 4 4 4 5

# You will get an error if the first element is NA!

# naToLastNonNa(c(NA, 1, NA, 2))

## Error in naToLastNonNa(c(NA, 1, NA, 2)) : 
##   The first element must not be NA
}

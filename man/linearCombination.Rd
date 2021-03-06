% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix.R
\name{linearCombination}
\alias{linearCombination}
\title{Linear Combination of a Matrix}
\usage{
linearCombination(x, coeffs, version = 1)
}
\arguments{
\item{x}{numeric matrix}

\item{coeffs}{numeric vector of coefficients}

\item{version}{1 or 2 (default: 1). Allows for two different versions of
calculation both of which should return the same!}
}
\description{
Calculate the linear combination of a matrix
}
\examples{
(x <- randomMatrix(c(4, 2)))
(coeffs <- rnorm(ncol(x)))
  
# Calculate the linear combination manually
LC1 <- x[, 1] * coeffs[1] + x[, 2] * coeffs[2]

# Caluclate with linearCombination()
LC2 <- linearCombination(x, coeffs)

# The result shoulc be the same!
all.equal(LC1, LC2) # TRUE

}

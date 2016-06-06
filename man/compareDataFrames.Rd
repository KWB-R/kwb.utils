\name{compareDataFrames}
\alias{compareDataFrames}
\title{compare two data frames by columns}
\description{compare two data frames by columns}
\usage{compareDataFrames(x, y)}
\arguments{
  \item{x}{first data frame}
  \item{y}{second data frame}
}

\value{list of logical}

\author{Hauke Sonnenberg}




\examples{
x <- data.frame(a = 1:2, b = 2:3)
y <- x

test1 <- all(unlist(compareDataFrames(x, y)))

z <- compareDataFrames(x, y[, c("b", "a")])
expectedFalse <- c("identical", "identicalExceptAttributes", "sameColumnNames")  
test2 <- all(names(which(!unlist(z))) == expectedFalse)

test1 && test2
}

\name{safeColumnBind}
\alias{safeColumnBind}
\title{cbind(x1, x2) or x2 if x1 is NULL}
\description{"Safe" version of cbind. If \emph{x1} is NULL \emph{x2} is returned otherwise
\code{cbind(x1, x2)}}
\usage{safeColumnBind(x1, x2)}
\arguments{
  \item{x1}{
}
  \item{x2}{
}
}

\value{result of \code{cbind(x1, x2)} or \emph{x2} if \emph{x1}
is null. }

\author{Hauke Sonnenberg}




\examples{
x1 <- NULL

for (i in 1:3) {
  
  x2 <- data.frame(a = 1:3, b = rnorm(3))
  x1 <- safeColumnBind(x1, x2)
  
  # using cbind would result in an error: 
  # x1 <- cbind(x1, x2)
}

x1
}

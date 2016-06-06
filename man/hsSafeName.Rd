\name{hsSafeName}
\alias{hsSafeName}
\title{Non-existing desired name}
\description{Returns a name that is not yet contained in a vector \emph{myNames} 
of existing names. }
\usage{hsSafeName(myName, myNames)}
\arguments{
  \item{myName}{desired name.}
  \item{myNames}{vector of existing names.}
}

\value{If \emph{myName} is not contained in \emph{myNames} it is returned. 
Otherwise \emph{myName} is modified to \emph{myName}_01, \emph{myName}_02, 
... until a non-existing name is found that is then returned.}

\author{Hauke Sonnenberg}




\examples{
existing <- c("a", "b")
myName <- hsSafeName("c", existing) 
myName                               # "c"
myName <- hsSafeName("a", existing) 
myName                               # "a_1"
hsSafeName("a", c(existing, myName)) # "a_2"
}

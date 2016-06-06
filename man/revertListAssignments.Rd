\name{revertListAssignments}
\alias{revertListAssignments}
\title{revertListAssignments}
\description{switch list elements with their names}
\usage{revertListAssignments(x)}
\arguments{
  \item{x}{list of named elements}
}

\value{list with the names of \emph{x} as elements and the elements of \emph{x}
as names}

\author{Hauke Sonnenberg}




\examples{
abbreviation <- list(de = "Germany", en = "England")

revertListAssignments(abbreviation)

## reverting twice results in the original list
identical(
  abbreviation, 
  revertListAssignments(revertListAssignments(abbreviation))
)
}

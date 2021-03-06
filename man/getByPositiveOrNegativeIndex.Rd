% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector.R
\name{getByPositiveOrNegativeIndex}
\alias{getByPositiveOrNegativeIndex}
\title{Get Vector Elements by Positive or Negative Index}
\usage{
getByPositiveOrNegativeIndex(elements, index)
}
\arguments{
\item{elements}{vector of elements}

\item{index}{positive or negative index(es) with absolute value between 1 and
length(\emph{elements})}
}
\value{
element(s) out of \emph{elements} corresponding to the index(es)
  given in \emph{index}
}
\description{
Get element from vector, counting from head or tail
}

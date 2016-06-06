\name{appendSuffix}
\alias{appendSuffix}
\title{append suffix to (selected) character values}
\description{append suffix to (selected) character values}
\usage{appendSuffix(values, suffix, valuesToOmit = NULL)}
\arguments{
  \item{values}{vector of character values to which \emph{suffix} is to be appended}
  \item{suffix}{(character) suffix to be pasted to \emph{values} that are not in
\emph{valuesToOmit}}
  \item{valuesToOmit}{vector of values in \emph{values} to which no suffix is to be appended}
}

\value{\emph{values} with \emph{suffix} appended to those values that are not in
\emph{valuesToOmit}}

\author{Hauke Sonnenberg}




\examples{
values <- c("a", "b", "c")

# Append ".1" to all values
appendSuffix(values, ".1")

# Append ".1" to all values but "c"
appendSuffix(values, ".1", valuesToOmit = "c")
}

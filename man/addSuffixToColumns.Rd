% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column.R
\name{addSuffixToColumns}
\alias{addSuffixToColumns}
\title{Add Suffix to Column Names}
\usage{
addSuffixToColumns(data, suffix, except = NULL)
}
\arguments{
\item{data}{data frame}

\item{suffix}{suffix to be added to each column name except the columns given
in \code{except}}

\item{except}{names of columns to which no suffix is to be given}
}
\value{
\code{data} with renamed columns
}
\description{
Add Suffix to Column Names
}
\examples{
d1 <- data.frame(id = 1, a = 2, b = 3)
d2 <- data.frame(id = 1, c = 2, d = 3)

# Using merge the origin of the column gets lost
merge(d1, d2)

# Add a suffix before merging
merge(
  addSuffixToColumns(d1, ".first", except = "id"), 
  addSuffixToColumns(d2, ".second", except = "id"), 
  by = "id"
)

}

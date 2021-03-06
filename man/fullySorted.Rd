% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataFrame.R
\name{fullySorted}
\alias{fullySorted}
\title{Sort a Data Frame by all of its Columns}
\usage{
fullySorted(x, decreasing = FALSE, ..., renumber.rows = TRUE)
}
\arguments{
\item{x}{data frame}

\item{decreasing}{passed to \code{\link[base]{order}}}

\item{...}{further arguments passed to \code{\link[base]{order}}}

\item{renumber.rows}{if \code{TRUE} (default) the rows in the returned data
frame are renumbered from 1 to the number of rows in \code{x}}
}
\description{
Sort a Data Frame by all of its Columns
}
\examples{
fullySorted(head(iris))
fullySorted(head(iris), decreasing = TRUE)
fullySorted(head(iris[, 5:1]))
fullySorted(head(iris[, 5:1]), decreasing = TRUE)

}

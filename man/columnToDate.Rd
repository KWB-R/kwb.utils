% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column.R
\name{columnToDate}
\alias{columnToDate}
\title{Convert Column in Data Frame To Date}
\usage{
columnToDate(df, column, dbg = TRUE)
}
\arguments{
\item{df}{data frame}

\item{column}{name of column in \code{x}}

\item{dbg}{if \code{TRUE} a debug message is shown}
}
\value{
\code{df} with \code{column} converted to class \code{Date} with
  \code{as.Date}
}
\description{
Convert Column in Data Frame To Date
}
\examples{
df <- data.frame(id = 1:2, date = c("2018-10-23", "2018-10-23"))
str(df)

df <- columnToDate(df, "date")
str(df)

}

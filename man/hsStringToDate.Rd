\name{hsStringToDate}
\alias{hsStringToDate}
\title{hsStringToDate}
\description{Convert date string to string and stop on failure}
\usage{hsStringToDate(strDate, dateFormat)}
\arguments{
  \item{strDate}{(vector of) string(s) representing date(s)}
  \item{dateFormat}{date format specifier describing the format in which dates are represented
in the csv file. Use placeholders , \code{"\%d"} (day), 
\code{"\%m"} (month), \code{"\%y"} (2-digit year), \code{"\%Y"}
(4-digit year) to describe the date format. \code{"\%d.\%m.\%Y"}, 
\code{"\%d/\%m/\%y"}, \code{"\%Y-\%m-\%d"} are examples for
valid format specifiers.}
}

\value{(vector of) Date object(s)}

\author{Hauke Sonnenberg}






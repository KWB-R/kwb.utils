% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string.R
\name{stringEndsWith}
\alias{stringEndsWith}
\title{stringEndsWith}
\usage{
stringEndsWith(x, endsWith)
}
\arguments{
\item{x}{vector of character to be checked if they end with \code{endsWith}}

\item{endsWith}{string to be searched for at the end of the string(s) in
\code{x}}
}
\description{
stringEndsWith
}
\examples{
stringEndsWith(c("abc", "Kabeljau", "Arabella"), "a")
stringEndsWith(c("abc", "Kabeljau", "Arabella"), "jau")
  
}

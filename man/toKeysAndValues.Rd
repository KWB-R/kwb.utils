% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{toKeysAndValues}
\alias{toKeysAndValues}
\title{Key Value String to List of Keys and Values}
\usage{
toKeysAndValues(x, separators = c(",", "="))
}
\arguments{
\item{x}{character vector of length 1}

\item{separators}{character vector of length 2 representing two types of
separators. The first (default: ",") is used to split \code{x} into single
\code{key = value} assignments. The second (default: "=") is used to split
each assignment into key and value.}
}
\value{
list with elements \code{keys} and \code{values}
}
\description{
Converts a string of the form "a=1,b=2" to a list with elements
  \code{keys} (here: c("a", "b")) and \code{values} (here: (1,2)).
}

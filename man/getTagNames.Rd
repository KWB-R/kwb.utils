% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.R
\name{getTagNames}
\alias{getTagNames}
\title{Find <tag>-tags in string}
\usage{
getTagNames(x, bt = c("<>", "[]")[1], dbg = FALSE, expected.length = length(x))
}
\arguments{
\item{x}{vector of character}

\item{bt}{bracket type, must be one of c("<>", "[]"). Default: "<>"}

\item{dbg}{if \code{TRUE} (default is \code{FALSE}) debug messages are shown}

\item{expected.length}{if given and different from the length of \code{x}
an error is thrown}
}
\description{
Return tags of the form <tag> that are contained in the string \emph{x}.
}

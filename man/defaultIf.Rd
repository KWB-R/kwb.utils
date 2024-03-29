% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default.R
\name{defaultIf}
\alias{defaultIf}
\title{Return a Default Value for Values Meeting a Condition}
\usage{
defaultIf(FUN.test, x, default, count = FALSE)
}
\arguments{
\item{FUN.test}{function returning vector of logical, to be applied to 
\code{x}}

\item{x}{vector in which to set default values}

\item{default}{value to be set in \code{x} at those positions where 
\code{FUN.text} returns \code{TRUE}}

\item{count}{if \code{TRUE} (the default is \code{FALSE}) the number of
replaced values is returned in the attributes \code{count}}
}
\description{
Return a Default Value for Values Meeting a Condition
}

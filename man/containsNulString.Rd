% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{containsNulString}
\alias{containsNulString}
\title{Check for nul String in File}
\usage{
containsNulString(filepath)
}
\arguments{
\item{filepath}{full path to file to be checked}
}
\value{
\code{TRUE} if first two bytes of file are \code{<FF><FE>}, else 
  \code{FALSE}
}
\description{
Check for nul String in File
}

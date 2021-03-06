% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{loadObject}
\alias{loadObject}
\title{Load R object from .RData file}
\usage{
loadObject(file, objectname = NULL, dbg = TRUE)
}
\arguments{
\item{file}{path to .RData file}

\item{objectname}{name of object to be loaded}

\item{dbg}{if \code{TRUE} a message about which object is loaded from which 
file is shown}
}
\value{
R object as specified in \emph{objectname}. If an object of that name does
  not exist in the .RData file an error is thrown
}
\description{
Load an R object of given name from a .RData file
}

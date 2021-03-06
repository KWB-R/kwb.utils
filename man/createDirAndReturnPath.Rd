% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system.R
\name{createDirAndReturnPath}
\alias{createDirAndReturnPath}
\title{Create a Directory including required "upward" Folders}
\usage{
createDirAndReturnPath(path, dbg = TRUE, confirm = FALSE)
}
\arguments{
\item{path}{character string representing the path to the directory to be
created}

\item{dbg}{if \code{TRUE} messages about created or found directories are
shown}

\item{confirm}{if \code{TRUE} (the default is \code{FALSE}!) the user is
asked to confirm the creation of a directory}
}
\value{
created path or \code{NULL} if the path could not be created
}
\description{
Create a Directory including required "upward" Folders
}

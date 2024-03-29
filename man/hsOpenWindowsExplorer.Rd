% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system.R
\name{hsOpenWindowsExplorer}
\alias{hsOpenWindowsExplorer}
\title{Open Windows Explorer}
\usage{
hsOpenWindowsExplorer(
  startdir = tempdir(),
  use.shell.exec = !isNetworkPath(startdir)
)
}
\arguments{
\item{startdir}{directory to be opened in Windows Explorer}

\item{use.shell.exec}{if \code{TRUE} \code{shell.exec} is used instead of
running the system command \code{cmd /C explorer}}
}
\description{
Open Windows Explorer
}

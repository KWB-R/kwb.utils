\name{roundColumns}
\alias{roundColumns}
\title{roundColumns}
\description{roundColumns}
\usage{roundColumns(dframe, columnNames = NULL, digits = NULL)}
\arguments{
  \item{dframe}{data frame containing numeric columns to be rounded}
  \item{columnNames}{names of (numeric) columns in \emph{dframe} to be rounded.}
  \item{digits}{number of digits to be rounded to (vector of length 1 expected) or list of
assignments in the form: \emph{columnName} = \emph{numberOfDigits}. If 
you give a list here, then there is no need to set the argument 
\emph{columnNames}  }
}

\value{\emph{dframe} with columns given in \emph{columnNames} being rounded to
\emph{digits} digits.}

\author{Hauke Sonnenberg}






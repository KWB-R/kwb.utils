\name{checkForMissingColumns}
\alias{checkForMissingColumns}
\title{Check for column existence}
\description{Stops if data frame \emph{frm} does not contain all columns of which the
names are given in \emph{reqCols}.}
\usage{checkForMissingColumns(frm, reqCols, do.stop = TRUE)}
\arguments{
  \item{frm}{data frame }
  \item{reqCols}{vector of names of which existence in \emph{frm} shall be checked}
  \item{do.stop}{if TRUE, stop() is called else warning() if a column is missing}
}

\value{TRUE if all required columns are available, else FALSE}

\author{Hauke Sonnenberg}






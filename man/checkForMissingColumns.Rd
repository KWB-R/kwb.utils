% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/column.R
\name{checkForMissingColumns}
\alias{checkForMissingColumns}
\title{Check for Column Existence}
\usage{
checkForMissingColumns(
  frm,
  reqCols,
  do.stop = TRUE,
  dataFrameName = deparse(substitute(frm))
)
}
\arguments{
\item{frm}{data frame}

\item{reqCols}{vector of names of which existence in \emph{frm} shall be
checked}

\item{do.stop}{if TRUE, stop() is called else warning() if a column is
missing}

\item{dataFrameName}{the name of the data frame to be shown in the error
message if a column was missing}
}
\value{
TRUE if all required columns are available, else FALSE
}
\description{
Stops if data frame \emph{frm} does not contain all columns of which the 
names are given in \emph{reqCols}.
}

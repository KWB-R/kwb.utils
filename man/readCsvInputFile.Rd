\name{readCsvInputFile}
\alias{readCsvInputFile}
\title{read CSV file}
\description{read CSV file giving column descriptions}
\usage{readCsvInputFile(csv, sep, dec, headerRow = 1, headerPattern = "", 
    columnDescription = NULL, maxRowToLookForHeader = 10, stopOnMissingColumns = TRUE, 
    encoding = "unknown", ...)}
\arguments{
  \item{csv}{full path to CSV file}
  \item{sep}{column separator}
  \item{dec}{decimal character}
  \item{headerRow}{number row in which the header  (containing column captions) is found}
  \item{headerPattern}{pattern matching the header row. If \emph{headerPattern} is given
\emph{headerRow} is not considered}
  \item{columnDescription}{list of column descriptors. The list elements are named with the name
of the list elements being the names that shall be used in the returned
data frame. Each list element is a list with elements \emph{match}
(pattern to be looked for in the header fields), ...}
  \item{maxRowToLookForHeader}{maximum number of rows to be considered when looking for the header row}
  \item{stopOnMissingColumns}{if TRUE (default) the program stops if not all columns defined in 
\emph{columnDescription} are found}
  \item{encoding}{passed to readLines, "Latin-1" or "UTF-8"}
  \item{\dots}{further arguments passed to read.table}
}



\author{Hauke Sonnenberg}






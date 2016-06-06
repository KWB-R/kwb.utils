\name{hsTrim}
\alias{hsTrim}
\title{Remove leading and trailing spaces}
\description{Remove leading, trailing (and, if requested, duplicate) spaces}
\usage{hsTrim(str, trim.multiple.spaces = TRUE)}
\arguments{
  \item{str}{vector of character containing the strings to be trimmed}
  \item{trim.multiple.spaces}{if TRUE (default), multiple consecutive spaces are replaced by one space}
}

\value{input string \emph{str} without leading or trailing spaces and with
multiple consecutive spaces being replaced by a single space}

\author{Hauke Sonnenberg}






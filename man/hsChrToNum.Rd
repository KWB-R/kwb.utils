\name{hsChrToNum}
\alias{hsChrToNum}
\title{hsChrToNum}
\description{conversion of text representing a number to a valid number}
\usage{hsChrToNum(x, country, stopOnError = TRUE)}
\arguments{
  \item{x}{(vector of) text value(s) to be converted to numeric}
  \item{country}{"en" if value(s) in \emph{x} is (are)  given in English format (decimal
point ".", thousands separator ",") or "de" if value is given in German
format (decimal point ",", thousands separator ".").}
  \item{stopOnError}{if TRUE (default) the program stops if any of the given values could not
be converted.}
}



\author{Hauke Sonnenberg}






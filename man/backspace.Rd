% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{backspace}
\alias{backspace}
\title{String of n Backspaces}
\usage{
backspace(n)
}
\arguments{
\item{n}{number of backspace characters}
}
\value{
vector of character of length one
}
\description{
String of n Backspaces
}
\examples{
update <- function(x) cat(backspace(3), x)
x <- "value: 123"
cat(x)
cat(paste0(x, backspace(3), "987"))

}

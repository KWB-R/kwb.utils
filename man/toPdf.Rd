% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf.R
\name{toPdf}
\alias{toPdf}
\title{Let Expressions Plot into a PDF File}
\usage{
toPdf(expressions, pdfFile = NULL, ...)
}
\arguments{
\item{expressions}{R expressions creating plots that are to be redirected 
into a pdf file. You may pass multiple expressions within opening and 
closing curly braces}

\item{pdfFile}{optional. Path to the PDF file to be created. The directory
part of the path must exist. If not given or \code{NULL}, the PDF file is
created in the \code{tempdir()} folder.}

\item{\dots}{further arguments passed to \code{\link{pdf}}}
}
\value{
The function returns the path to the created PDF file.
}
\description{
The function opens a PDF device with \code{\link{pdf}}, executes the given 
expressions, closes the PDF file and displays the file in a PDF viewer.
}
\examples{
\dontrun{
toPdf({
  plot(1:10, 10:1)
  barplot(1:10)
  hist(rnorm(100))
})
}
}

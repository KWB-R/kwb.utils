\name{hsShowPdf}
\alias{hsShowPdf}
\title{Open PDF file in PDF viewer}
\description{Opens the PDF file of which the full path is given in \emph{Pdf} in a
PDF viewer.}
\usage{hsShowPdf(Pdf, dbg = TRUE)}
\arguments{
  \item{Pdf}{full path to PDF file}
  \item{dbg}{
}
}



\author{Hauke Sonnenberg}



\seealso{\code{\link{hsPrepPdf}}}
\examples{

# Set path to PDF file and open PDF device
tmpPdf <- tempfile("ex_hsFinishPdf", fileext = ".pdf") 
hsPrepPdf(tmpPdf)

## Plot something
plot(x <- seq(-pi,pi,pi/100), sin(x), type = "l")

## Finish PDF file.
dev.off()

## Open PDF file in viewer.
hsShowPdf(tmpPdf)
}

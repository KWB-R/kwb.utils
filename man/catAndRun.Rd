% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{catAndRun}
\alias{catAndRun}
\title{Print Debug Messages Before and After Running Code}
\usage{
catAndRun(
  messageText = "Running code",
  expr,
  newLine = 2L,
  dbg = TRUE,
  log_time = TRUE
)
}
\arguments{
\item{messageText}{text to be printed before running the code}

\item{expr}{expressions to be run. Enclose more than one expression in
curly braces}

\item{newLine}{integer controlling new lines. 0: no extra new line, 1:
new line after \code{messageText}, 2: new line after "ok.", 3: new line
after both, \code{messageText} and "ok."}

\item{dbg}{logical. If \code{FALSE}, output is suppressed.}

\item{log_time}{logical. If \code{TRUE}, the time elapsed during the 
evaluation of \code{expr} is printed.}
}
\value{
This function returns the evaluation of \code{expr}. The result is
  returned invisibly so that the result of an assignment as the last 
  expression in \code{exprt} does not show up on the console.
}
\description{
Print Debug Messages Before and After Running Code
}
\examples{
for (newLine in 0:3) {

  catAndRun("work hard", newLine = newLine, {
    cat("hard\nworking\n")
  })

  cat("here.\n\n")
}

}

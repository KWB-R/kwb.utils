\name{multiSubstitute}
\alias{multiSubstitute}
\title{multiple substitutions}
\description{apply multiple substitutions on a vector of character. For each element in
\emph{replacements} gsub is called with the element name being the pattern
and the element value being the replacement.}
\usage{multiSubstitute(strings, replacements, ...)}
\arguments{
  \item{strings}{vector of character}
  \item{replacements}{list of pattern = replacement pairs. }
  \item{\dots}{additional arguments passed to gsub}
}



\author{Hauke Sonnenberg}






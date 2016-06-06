\name{getFunctionValueOrDefault}
\alias{getFunctionValueOrDefault}
\title{take function value or default if NA}
\description{take the function value or a default value if the function value is NA}
\usage{getFunctionValueOrDefault(values, FUN, default, warningMessage = NA)}
\arguments{
  \item{values}{vector of values given to \emph{FUN}}
  \item{FUN}{function to which values are passed and which offers the argument "na.rm"}
  \item{default}{default value to be returned if all values are NA}
  \item{warningMessage}{Warning message given if the default was taken}
}



\author{Hauke Sonnenberg}






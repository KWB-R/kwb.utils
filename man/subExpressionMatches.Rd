\name{subExpressionMatches}
\alias{subExpressionMatches}
\title{subExpressionMatches}
\description{subExpressionMatches}
\usage{subExpressionMatches(regularExpression, text, match.names = NULL, 
    select = structure(seq_along(match.names), names = match.names), 
    simplify = TRUE)}
\arguments{
  \item{regularExpression}{regular expression containing parts in parentheses that are to be 
extracted from \emph{text}}
  \item{text}{text to be matched against the regular expression}
  \item{match.names}{optional. Names that are to be given to the extracted parts in the result
list,}
  \item{select}{named vector of numbers specifying the subexpressions in parentheses to
be extracted.}
  \item{simplify}{if TRUE (default) and \emph{text} has only one element, the output 
structure will be a list instead a list of lists}
}

\value{If \code{length(text) > 1} a list is returned with as many 
elements as there are strings in \emph{text} each of which is itself a 
list containing the strings matching the subpatterns (enclosed in 
parentheses in \emph{regularExpression}) or NULL for strings that did
not match. If \emph{match.names} are given, the elements of these lists
are named according to the names given in \emph{match.names}. If
\emph{text} is of length 1 and \emph{simplify} = TRUE (default) the top
level list structure described above is omitted, i.e. the list of
substrings matching the subpatterns is returned.}

\author{Hauke Sonnenberg}




\examples{
# split date into year, month and day
subExpressionMatches("(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23")

# split date into year, month and day (give names to the resulting elements)
x <- subExpressionMatches(
  regularExpression = "(\\\\d{4})\\\\-(\\\\d{2})\\\\-(\\\\d{2})", "2014-04-23",
  match.names = c("year", "month", "day")
)

cat(paste("Today is ", x$day, "/", x$month, " of ", x$year, "\n", sep=""))
}

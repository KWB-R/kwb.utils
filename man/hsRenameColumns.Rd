\name{hsRenameColumns}
\alias{hsRenameColumns}
\title{rename columns in a data frame}
\description{rename columns in a data frame giving tupels of original name
and substitute name as named elements in list "renames"}
\usage{hsRenameColumns(dframe, renames)}
\arguments{
  \item{dframe}{data.frame, }
  \item{renames}{list with named elements each of which defines a column rename in the form
<old-name> = <new-name>}
}

\value{\emph{dframe} with columns renamed as specified in \emph{renames}}

\author{Hauke Sonnenberg}






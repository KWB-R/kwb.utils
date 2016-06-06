\name{removeColumns}
\alias{removeColumns}
\title{remove columns from data frame}
\description{remove columns from a data frame}
\usage{removeColumns(dframe, columnsToRemove, drop = FALSE)}
\arguments{
  \item{dframe}{data frame, }
  \item{columnsToRemove}{vector of column names giving the columns to remove}
  \item{drop}{if FALSE, a data frame is returned in any case, otherwise the result may
be a vector if only one column remains}
}

\value{\emph{dframe} with columns given in \emph{columnsToRemove} being removed.
User attributes of \emph{dframe} are restored.}

\author{Hauke Sonnenberg}






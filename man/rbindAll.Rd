% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataFrame.R
\name{rbindAll}
\alias{rbindAll}
\title{rbind all data frames given in a list}
\usage{
rbindAll(x, nameColumn = "", remove.row.names = TRUE, namesAsFactor = TRUE)
}
\arguments{
\item{x}{list of data frames to be passed to \code{rbind}}

\item{nameColumn}{optional. If given, an additional column of that name is added to the
resulting data frame containing the name (or number if \emph{args} is
an unnamed list) of the element in \emph{x} that the corresponding rows
belong to}

\item{remove.row.names}{if TRUE (default) row names are reset in the output data frame}

\item{namesAsFactor}{if TRUE (default) and \emph{nameColumn} is given the values in
column \emph{nameColumn} are converted to a factor}
}
\description{
rbind all data frames given in a list
}
\examples{
L <- list(
  A = data.frame(x = 1:2, y = 2:3),
  B = data.frame(x = 1:3, y = 2:4)
)

L.unnamed <- L
names(L.unnamed) <- NULL

y1 <- rbindAll(L)
y2 <- rbindAll(L, nameColumn = "group")
y3 <- rbindAll(L.unnamed, nameColumn = "group", namesAsFactor = FALSE)
y4 <- rbindAll(L.unnamed, nameColumn = "group")

expected1 <- data.frame(
  x = c(L$A$x, L$B$x),
  y = c(L$A$y, L$B$y)
)

expected2 <- cbind(
  expected1,
  group = as.factor(c(rep("A", nrow(L$A)), rep("B", nrow(L$B)))),
  stringsAsFactors = FALSE
)

expected3 <- cbind(
  expected1,
  group = c(rep(1L, nrow(L$A)), rep(2L, nrow(L$B)))
)

expected4 <- expected3
expected4$group <- as.factor(expected4$group)

identical(y1, expected1) &&
  identical(y2, expected2) &&
  identical(y3, expected3) &&
  identical(y4, expected4)

}

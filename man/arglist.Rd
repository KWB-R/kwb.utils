\name{arglist}
\alias{arglist}
\title{merge argument lists or arguments}
\description{creates a list of arguments from given argument lists and arguments.
This function allows to create argument lists for function calls. You may
start with some basic argument list and then merge other argument lists or
single argument assignments into this list. Merging means that elements of
the same name are overriden and elements with new names are appended.}
\usage{arglist(..., warn.on.NULL = FALSE)}
\arguments{
  \item{\dots}{list of arguments to this function. All unnamed arguments are assumed to
be argument lists which are merged using \code{\link{merge.lists}} first. 
All named arguments are then merged into this list.}
  \item{warn.on.NULL}{if TRUE (default is FALSE) a warning is given if any of the arguments
given to this function is NULL}
}

\value{merged list of arguments}

\author{Hauke Sonnenberg}



\seealso{\code{\link{callWith}}}
\examples{
# define some default arguments
args.default <- list(xlim = c(0, 10), ylim = c(0, 10), col = "red", lwd = 2)

# call plot with the default arguments
do.call(plot, arglist(args.default, x = 1:10))

# call plot with the default arguments but override the colour
do.call(plot, arglist(args.default, x = 1:10, col = "blue"))
}

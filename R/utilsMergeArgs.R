# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  stopifnot(identical(
    arglist(a = 1, b = 2, c = 3),
    list(a = 1, b = 2,c = 3)
  ))

  stopifnot(identical(
    arglist(list(a = 1, b = 2), list(b = 3), c = 4),
    list(a = 1, b = 3, c = 4)
  ))
  
  stopifnot(identical(
    arglist(list(xlim = c(0, 20), ylim = c(0, 100)), xlim = c(20, 40)),
    list(xlim = c(20, 40), ylim = c(0, 100))
  ))  
  
  stopifnot(identical(
    arglist(list(xlim = c(0, 20), ylim = c(0, 100)), zlim = c(-1, 1)),
    list(xlim = c(0, 20), ylim = c(0, 100), zlim = c(-1, 1))
  ))
}

# callWith ---------------------------------------------------------------------
callWith <- structure(
  function # call a function with given arguments
### call a function with the given arguments. Unnamed arguments are expected to
### be lists containing further argument assignments. Multiple argument lists
### are merged using \code{\link{arglist}} in the order of their appearence.
(
  FUN, ...
)
{
  ##seealso<< \code{\link{arglist}}
  do.call(FUN, arglist(...))
  ### the return value is the return value of the function \code{FUN}.
}, ex = function() {
  # define some default arguments
  args.default <- list(xlim = c(0, 10), ylim = c(0, 10), col = "red", lwd = 2)
  
  # call plot with the default arguments
  callWith(plot, x = 1:10, args.default)
  
  # call plot with the default arguments but override the colour
  callWith(plot, x = 1:10, args.default, col = "blue")
})

# arglist -----------------------------------------------------------------------
arglist <- structure(
  function # merge argument lists or arguments
### creates a list of arguments from given argument lists and arguments.
### This function allows to create argument lists for function calls. You may
### start with some basic argument list and then merge other argument lists or
### single argument assignments into this list. Merging means that elements of
### the same name are overriden and elements with new names are appended.
(
  ...,
  ### list of arguments to this function. All unnamed arguments are assumed to
  ### be argument lists which are merged using \code{\link{merge.lists}} first. 
  ### All named arguments are then merged into this list.
  warn.on.NULL = FALSE
  ### if TRUE (default is FALSE) a warning is given if any of the arguments
  ### given to this function is NULL
)
{
  ##seealso<< \code{\link{callWith}}
  args <- list(...)
  
  unnamed <- is.unnamed(args)
  
  named.args <- args[! unnamed]
  
  if (any(unnamed)) {
    
    # merge all argument lists
    relisted <- do.call(merge.lists, c(args[unnamed], warn.on.NULL = warn.on.NULL))
    
    named.args <- merge.lists(relisted, named.args, warn.on.NULL = warn.on.NULL)
  } 

  named.args
  ### merged list of arguments
}, ex = function() {
  # define some default arguments
  args.default <- list(xlim = c(0, 10), ylim = c(0, 10), col = "red", lwd = 2)
  
  # call plot with the default arguments
  do.call(plot, arglist(args.default, x = 1:10))
  
  # call plot with the default arguments but override the colour
  do.call(plot, arglist(args.default, x = 1:10, col = "blue"))
})

# is.unnamed -------------------------------------------------------------------
is.unnamed <- structure(
  function # are list elements unnamed?
### returns a vector of logical as long as \emph{x} holding TRUE at indices
### where the list element at the same indices are named and FALSE at positions
### where the list element at the same indices are not named.
(
  x
  ### list
)
{
  stopifnot(is.list(x))
  
  names.x <- names(x)
  
  if (is.null(names.x)) {
    rep(TRUE, length(x))
  } 
  else {
    names.x == ""
  }
  
  ### vector of logical
}, ex = function() {
  is.unnamed(list(1, b = 2)) # TRUE FALSE
  is.unnamed(list(a = 1, 2)) # FALSE TRUE
  is.unnamed(list()) # logical(0)
  is.unnamed(list(a = 1, 2, c = 3)) # FALSE  TRUE FALSE
})

# merge.lists ------------------------------------------------------------------
merge.lists <- structure(
  function # merge lists overriding elements of the same name
### merge lists overriding elements of the same name
(
  ...,
  ### lists
  warn.on.NULL = TRUE
  ### if TRUE (default) a warning is given if any of the arguments given to
  ### this function is NULL
)
{
  ##seealso<< \code{\link{arglist}}
  
  lists <- list(...)

  # Check for NULL elements
  isNull <- sapply(lists, is.null)
  
  if (any(isNull) && isTRUE(warn.on.NULL)) {
    plural <- sum(isNull) > 1
    argvalues <- sub("^list", "", deparse(substitute(lists)))
    warning(sprintf("%d argument%s given to merge.lists%s %s NULL",
                    sum(isNull), 
                    ifelse(plural, "s", ""), 
                    argvalues,
                    ifelse(plural, "are" ,"is")))
  }
  
  # Continue only with the non-NULL lists
  lists <- lists[! isNull]
  
  # All (non-NULL) arguments must be lists
  isList <- sapply(lists, is.list)
  
  if (! all(isList)) {
    stop("Not all (non-NULL) arguments are lists!")
  }
  
  N <- length(lists)
  
  if (N == 0) {
    relisted <- list()
  }
  else if (N == 1) {
    relisted <- lists[[1]]
  } 
  else {
    relisted <- .merge.2.lists(lists[[1]], lists[[2]])

    if (N > 2) {
      relisted <- do.call(merge.lists, c(list(relisted), lists[-(1:2)]))
    }
  } 

  relisted
  ### list containing the elements given in \code{...} 
}, ex = function() {
  # merge two lists with different elements
  merge.lists(list(a = 1), list(b = 2))
  
  # merge two lists with one element of the same name: override element "b"
  merge.lists(list(a = 1, b = 2), list(b = 3, c = 4))
})

# .merge.2.lists ---------------------------------------------------------------
.merge.2.lists <- function(list.1, list.2)
{
  stopifnot(is.list(list.1))
  stopifnot(is.list(list.2))
  
  names.1 <- names(list.1)
  names.2 <- names(list.2)
  
  common.names <- intersect(names.1, names.2)
  
  list.1[common.names] <- list.2[common.names]
  
  c(list.1, list.2[setdiff(names.2, names.1)])
}

# reproducibleSample -----------------------------------------------------------

#' Reproducible Call to the Sample Function
#' 
#' This function calls \code{\link{sample}} and stores the values that are
#' required to reproduce the exact same sampling in the attribute 
#' \code{random_seed} of the returned vector. When this attribute is passed
#' to another call of this function, the values returned will be the same as
#' in the first call.
#' 
#' @param \dots arguments passed to \code{FUN}.
#' @param FUN the sample function to be called. Default: \code{\link{sample}}.
#' @param random_seed vector of integer as stored in \code{.Random.seed}.
#' @return This function returns what \code{\link{sample}} returns with an 
#'   attribute \code{random_seed} attached.
#' @export
#' @examples
#' # Take a sample
#' x <- reproducibleSample(1:100, 10)
#' x
#' 
#' # The full seed vector is returned in the attribute "random_seed"
#' random_seed <- attr(x, "random_seed")
#' 
#' # Take a new sample, this time passing the seed vector
#' y <- reproducibleSample(1:100, 10, random_seed = random_seed)
#' y
#' 
#' # The values are identical to the values of the first sampling
#' identical(x, y)
#' 
reproducibleSample <- function(..., FUN = sample, random_seed = NULL)
{
  if (is.null(random_seed)) {
    
    if (! exists(".Random.seed")) {
      set.seed(NULL)
      stopifnot(exists(".Random.seed"))
    }
    
    random_seed <- .Random.seed

  } else {
    
    .Random.seed <<- random_seed
  }
  
  structure(
    FUN(...), 
    random_seed = invisible(random_seed),
    class = "repro_sample"
  )
}

#' Print Method for Object of Class "repro_sample"
#' 
#' @param x object to be printed
#' @param \dots further arguments, not used.
#' @export
print.repro_sample <- function(x, ...)
{
  print.default(structure(x, random_seed = NULL, class = NULL))
}

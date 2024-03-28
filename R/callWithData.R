# callWithData -----------------------------------------------------------------

#' Call a Function with Argument Combinations from a Data Frame
#'
#' @param FUN function to be called
#' @param data data frame with one column per argument of \code{FUN}
#' @param \dots further (constant) arguments to \code{FUN} that are passed to
#'   \code{\link{mapply}} via \code{MoreArgs}
#' @param threshold if the ratio of unique value combinations in the relevant
#'   columns in data to all value combinations in these columns is below this
#'   threshold value then FUN will be called only with the unique value
#'   combinations. This should increase performance.
#' @param SIMPLIFY passed to\code{\link{mapply}}, default: \code{TRUE}
#' @param USE.NAMES passed to\code{\link{mapply}}, default: \code{TRUE}
#' @return vector of length \code{nrow(data)} with the result values returned by
#'   \code{FUN}
#' @importFrom methods formalArgs
#' @export
#' @examples
#' combis <- expand.grid(x = 1:2, y = c(10, 20, 30))
#' combis
#'
#' callWithData(`+`, combis)
callWithData <- function(
    FUN,
    data,
    ...,
    threshold = 0.5,
    SIMPLIFY = TRUE,
    USE.NAMES = TRUE
)
{
  # What arguments does FUN have?
  all_args <- methods::formalArgs(FUN)

  # Select the columns from data that are arguments of FUN
  arg_data <- selectColumns(data, intersect(all_args, names(data)))

  # Split arg_data into sets of identical rows
  sets <- splitIntoIdenticalRows(arg_data)

  # Number of all value combinations
  n_all <- nrow(arg_data)

  # Number of unique value combinations
  n_unique <- length(sets)

  # Should we run FUN only for the unique value combinations?
  run_unique <- (n_unique / n_all < threshold)

  # Name of the function to be called
  fun_name <- deparse(substitute(FUN))

  # Run FUN for each row of run_data
  results <- catAndRun(
    messageText = if (run_unique) {
      sprintf(
        "-> Calling %s() for %d unique value combinations",
        fun_name, n_unique
      )
    } else {
      sprintf(
        "-> Calling %s() for all %d value combinations",
        fun_name, n_all
      )
    },
    expr = {

      more_args <- list(...)

      mapply_args_fix <- list(
        FUN = FUN,
        MoreArgs = if (length(more_args)) more_args,
        SIMPLIFY = SIMPLIFY,
        USE.NAMES = USE.NAMES
      )

      mapply_args_var <- if (run_unique) {
        removeColumns(rbindFirstRows(sets), "row.")
      } else {
        arg_data
      }

      do.call(mapply, c(mapply_args_fix, mapply_args_var))
    }
  )

  if (!run_unique) {
    return(results)
  }

  catAndRun(
    "-> Expanding the results to the extent of the input",
    expandToVector(
      x = results,
      indices = lapply(sets, selectColumns, "row.")
    )
  )
}

# splitIntoIdenticalRows -------------------------------------------------------
splitIntoIdenticalRows <- function(data)
{
  split(cbind(data, row. = seqAlongRows(data)), f = data, drop = TRUE)
}

# seqAlongRows ---------------------------------------------------------------
seqAlongRows <- function(data)
{
  seq_len(nrow(data))
}

# rbindFirstRows ---------------------------------------------------------------
rbindFirstRows <- function(x)
{
  stopifnot(is.list(x), all(sapply(x, nDims) == 2L))
  
  resetRowNames(do.call(rbind, lapply(x, utils::head, 1L)))
}

# nDims: number of dimensions --------------------------------------------------
nDims <- function(x)
{
  length(dim(x))
}

# expandToVector ---------------------------------------------------------------
expandToVector <- function(x, indices)
{
  stopifnot(length(x) == length(indices))
  
  result <- list()
  
  result[unlist(indices)] <- rep(x, lengths(indices))
  
  result
}

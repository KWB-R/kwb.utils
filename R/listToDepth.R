# listToDepth ------------------------------------------------------------------

#' List Elements Recursively up to Depth
#' 
#' @param path path to the element at which to start listing
#' @param recursive If \code{TRUE}, the listing is performed recursively, up to
#'   a depth level of \code{max_depth}
#' @param max_depth maximal depth of level to be listed 
#'   if \code{recursive = TRUE} (0 = no recursion at all)
#' @param full_info return only \code{path} and \code{isdir} information or
#'   the full information provided by \code{FUN(full_info = TRUE)}?
#' @param FUN function called to get the listing of the element given in
#'   \code{path}. The function must accept a path as its first argument and it
#'   must define the argument \code{full_info} second. It may accept further
#'   arguments. It must always return a data frame. For \code{full_info = FALSE}
#'   the data frame must have columns \code{file} and \code{isdir} (is the
#'   "file" a directory?). For \code{full_info = TRUE} the function may return
#'   further columns. The function must provide an empty data frame with the
#'   expected columns when being called with \code{character()} as the first
#'   argument. See \code{kwb.utils:::listFiles} for an example implementation
#'   that somehow simulates the behaviour of the \code{\link{dir}} function.
#'   See \code{kwb.dwd::list_url()} for a more advanced usage of this function
#'   in order to recursively list the files on an FTP server (FTP = file 
#'   transfer protocol).
#' @param \dots further arguments passed to \code{FUN}
#' @param depth start depth when \code{recursive = TRUE}, for internal use!
#' @param prob_mutate for internal use!
#' @return data frame containing at least the columns \code{file} and 
#'   \code{isdir}. If \code{full_info = TRUE} the result data frame may contain
#'   further columns, as provided by the function given in \code{FUN} for
#'   \code{full_info = TRUE}.
#' @examples 
#' # Example list function provided in this package (file listing)
#' FUN <- kwb.utils:::listFiles
#' 
#' # The list function must provide empty records when no path is given. The 
#' # returned data frame must provide the columns "file" and "isdir" ...
#' FUN()
#' FUN(full_info = TRUE)
#' 
#' # ... even when being called with an empty character vector
#' FUN(character())
#' FUN(character(), full_info = TRUE)
#' 
#' # Call the function recursively up to the given depth level
#' kwb.utils:::listToDepth(".", max_depth = 1, FUN = FUN)
#' 
listToDepth <- function(
  path, 
  recursive = TRUE, 
  max_depth = 1, 
  full_info = FALSE, 
  FUN = listFiles, 
  ..., 
  depth = 0, 
  prob_mutate = 0
)
{
  # Helper function to mutate the path (to test failures!)
  mutate_or_not <- function(x, prob = 0.1) {
    stopifnot(inRange(prob, 0, 1))
    # Mutate with a probability of "prob"
    if (prob > 0 && sample(c(TRUE, FALSE), 1L, prob = c(prob, 1 - prob))) {
      # Add some nonsense
      x <- paste0(x, "blabla")
    }
    x
  }
  
  # kwb.utils::assignPackageObjects("kwb.utils")
  # kwb.utils::assignArgumentDefaults(listToDepth)
  # max_depth = 1;full_info=TRUE;set.seed(1)

  # Call the domain specific function list_contents(). The function is expected
  # to set the attribute "failed" to the given path in case that the path failed
  # to be accessed.
  info <- FUN(mutate_or_not(path, prob_mutate), full_info, ...)
  #info <- FUN(mutate_or_not(path, prob_mutate), full_info)

  # Helper function to get an info column
  get_info <- function(x) kwb.utils::selectColumns(info, x)

  # Which files represent directories?
  is_directory <- get_info("isdir")

  # Are we already at maximum depth?
  at_max_depth <- ! is.na(max_depth) && (depth == max_depth)

  # Return the file list if no recursive listing is requested or if we are
  # already at maximum depth or if there are no directories. The function is
  # also returned from if info is empty (! any(is_directory) is TRUE).
  if (! recursive || at_max_depth || ! any(is_directory)) {
    return(info)
  }

  # URLs representing directories
  directories <- get_info("file")[is_directory]

  # Number of directories
  n_directories <- length(directories)

  # There must be directories if we arrive here!
  stopifnot(n_directories > 0L)

  # Indices to loop through
  indices <- stats::setNames(seq_along(directories), directories)

  # List all directories by calling this function recursively
  subdir_infos <- lapply(indices, function(i) {

    #i <- 1L

    # Show the progress
    cat(sprintf("%s%d/%d: ", space(depth), i, n_directories))

    # Recursive call of this function
    listToDepth(
      path = paste0(kwb.utils::assertFinalSlash(path), directories[i]),
      recursive = recursive,
      max_depth = max_depth,
      full_info = full_info,
      FUN = FUN,
      ...,
      depth = depth + 1,
      prob_mutate = prob_mutate
    )
  })

  # We need a template just in case that no data could be retrieved
  template <- FUN(full_info = full_info)

  # Merge data frames with info on files in subdirectories
  subdir_info <- mergeFileInfos(subdir_infos, template)

  # Prepend info on files at this level
  result <- rbind(info[! is_directory, ], subdir_info)

  # Collect information on URLs that failed to be accessed
  failed <- c(attr(info, "failed"), attr(subdir_info, "failed"))

  # Return the sorted file information with newly composed attribute "failed"
  structure(orderBy(result, "file"), failed = failed)
}

# mergeFileInfos ---------------------------------------------------------------
mergeFileInfos <- function(file_infos, template)
{
  stopifnot(is.list(file_infos))

  # Keep only non-empty data frames
  dfs <- file_infos[sapply(file_infos, nrow) > 0L]

  # Function to prepend a parent name "p" to column "file" in data frame "df"
  prepend_parent <- function(df, p) {
    parent <- kwb.utils::assertFinalSlash(p)
    child <- kwb.utils::selectColumns(df, "file")
    kwb.utils::setColumns(df, file = paste0(parent, child), dbg = FALSE)
  }

  # Prepend the parent names to the filenames for the remaining data frames
  result <- do.call(rbind, mapply(
    prepend_parent, dfs, names(dfs), SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))

  # If the result is NULL (no data frames to loop through) set the result to the
  # empty file info record
  result <- kwb.utils::defaultIfNULL(result, template)

  # Collect the information on URLs that could not be listed
  failed <- unlist(excludeNULL(lapply(file_infos, attr, "failed"), FALSE))

  # Merge the file lists returned for each directory
  # Return the vector of files with an attribute "failed" holding the merged
  # URLs of directories that could not be read
  structure(result, failed = failed)
}

# listFiles --------------------------------------------------------------------
listFiles <- function(path = ".", full_info = FALSE, ...)
{
  message("listing ", path)
  
  # Return empty data frame if path is empty
  if (length(path) == 0L) {
    return(listFiles(full_info = full_info)[FALSE, ])
  }
  
  files <- dir(path, include.dirs = TRUE)
  
  result <- resetRowNames(file.info(file.path(path, files)))
  
  result$file <- files

  FUN <- if (full_info) moveColumnsToFront else selectColumns
  
  FUN(result, c("file", "isdir"))
}

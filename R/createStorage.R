# createStorage ----------------------------------------------------------------

#' Create Storage Object to Save/Load RDS files
#' 
#' This function returns an object that implements the methods \code{list()}, 
#' \code{save()}, \code{load()}, \code{remove()} that list, store, load or 
#' remove, respectively RDS files. The object is associated to a folder that 
#' is given to this function.
#' 
#' @param path path to the folder in which to store the RDS files. If the folder 
#'   does not exist it is attempted to be created. 
#' @return list with "member functions" \code{list()}, \code{save()}, 
#'   \code{load()}, \code{remove()}
#' @export
#' @examples
#' # Create a storage object
#' storage <- createStorage(tempdir())
#' 
#' # At the beginning, the storage is empty
#' storage$list()
#' 
#' # Store objects
#' a <- 2
#' b <- "two"
#' storage$save(a, b, c = list("Hello", "World"))
#' 
#' # List the objects
#' storage$list()
#' 
#' # Retrieve an object
#' storage$load("c")
#' 
#' # Remove objects
#' storage$remove("a")
#' storage$remove("b")
#' storage$remove("c")
#' 
#' # The storage is empty again
#' storage$list()
#' 
createStorage <- function(path)
{
  kwb.utils::createDirectory(path)

  object_path <- function(name) file.path(path, paste0(name, ".rds"))

  get_named_arg_assignments <- function() {
    args <- as.list(sys.call(which = 1L)[-1L])
    arg_names <- defaultIfNULL(names(args), character(length(args)))
    unnamed <- arg_names == ""
    arg_names[unnamed] <- lapply(args[unnamed], as.character)
    stats::setNames(lapply(args, eval), arg_names)
  }

  member_list <- function(full.names = FALSE) {
    dir(path, pattern = "\\.rds$", full.names = full.names, all.files = TRUE)
  }

  member_save <- function(..., .overwrite = FALSE) {

    assignments <- get_named_arg_assignments()
    assignments <- assignments[names(assignments) != ".overwrite"]
    stopifnot(all(nzchar(defaultIfNULL(names(assignments), ""))))

    for (name in names(assignments)) {

      file <- object_path(name)

      if (file.exists(file)) {

        if (! .overwrite) {
          message(
            "File exists:\n  ", file, "\n",
            "Use .overwrite = TRUE to overwrite. Returning NULL."
          )
          return(NULL)
        }

        message("Overwriting:\n  ", file)
      }

      saveRDS(assignments[[name]], file = file)
    }
  }

  member_load <- function(name) {

    file <- object_path(name)

    if (! file.exists(file)) {
      message("No such file:\n  ", file, ".\nReturning NULL.")
      return(NULL)
    }

    readRDS(file)
  }

  member_remove <- function(name) {

    file <- object_path(name)

    if (! file.exists(file)) {
      message("Nothing to delete. No such file:\n  ", file)
      return(NULL)
    }

    unlink(file)
  }

  list(
    list = member_list,
    save = member_save,
    load = member_load,
    remove = member_remove
  )
}
